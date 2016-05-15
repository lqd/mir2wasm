use error::*;
use rustc::mir::repr::*;
use rustc::mir::mir_map::MirMap;
use rustc::middle::const_val::ConstVal;
use rustc_const_math::{ConstInt, ConstIsize};
use rustc::ty::{self, TyCtxt, Ty, FnSig};
use rustc::hir::intravisit::{self, Visitor, FnKind};
use rustc::hir::{FnDecl, Block};
use syntax::ast::{NodeId, IntTy, UintTy, FloatTy};
use syntax::codemap::Span;
use std::ffi::CString;
use std::ptr;
use std::collections::HashMap;
use binaryen::*;

pub fn trans_crate<'tcx>(tcx: &TyCtxt<'tcx>,
                             mir_map: &MirMap<'tcx>) -> Result<()> {

    let _ignore = tcx.dep_graph.in_ignore();

    let ref mut v = BinaryenModuleCtxt {
        tcx: tcx,
        mir_map: mir_map,
        module: unsafe { BinaryenModuleCreate() },
        fun_types: HashMap::new(),
        c_strings: Vec::new(),
    };

    tcx.map.krate().visit_all_items(v);

    unsafe {
        BinaryenModulePrint(v.module);
    }

    Ok(())
}

struct BinaryenModuleCtxt<'v, 'tcx: 'v> {
    tcx: &'v TyCtxt<'tcx>,
    mir_map: &'v MirMap<'tcx>,
    module: BinaryenModuleRef,
    fun_types: HashMap<ty::FnSig<'tcx>, BinaryenFunctionTypeRef>,
    c_strings: Vec<CString>,
}

impl<'v, 'tcx: 'v> Drop for BinaryenModuleCtxt<'v, 'tcx> {
    fn drop(&mut self) {
        unsafe { BinaryenModuleDispose(self.module) };
    }
}

impl<'v, 'tcx> Visitor<'v> for BinaryenModuleCtxt<'v, 'tcx> {
    fn visit_fn(&mut self, fk: FnKind<'v>, fd: &'v FnDecl,
                b: &'v Block, s: Span, id: NodeId) {
        let mir = &self.mir_map.map[&id];

        let did = self.tcx.map.local_def_id(id);
        let ty = self.tcx.lookup_item_type(did).ty;
        let sig = ty.fn_sig().skip_binder();

        {
            let mut ctxt = BinaryenFnCtxt {
                tcx: self.tcx,
                mir: mir,
                id: id,
                sig: &sig,
                module: self.module,
                fun_types: &mut self.fun_types,
                c_strings: &mut self.c_strings,
            };

            ctxt.trans();
        }

        intravisit::walk_fn(self, fk, fd, b, s)
    }
}

struct BinaryenFnCtxt<'v, 'tcx: 'v> {
    tcx: &'v TyCtxt<'tcx>,
    mir: &'v Mir<'tcx>,
    id: NodeId,
    sig: &'tcx FnSig<'tcx>,
    module: BinaryenModuleRef,
    fun_types: &'v mut HashMap<ty::FnSig<'tcx>, BinaryenFunctionTypeRef>,
    c_strings: &'v mut Vec<CString>,
}

impl<'v, 'tcx: 'v> BinaryenFnCtxt<'v, 'tcx> {
    fn trans(&mut self) {

        let binaryen_args: Vec<_> = self.sig.inputs.iter().map(|t| rust_ty_to_binaryen(t)).collect();
        let mut needs_ret_local = false;
        let binaryen_ret = match self.sig.output {
            ty::FnOutput::FnConverging(t) => {
                if !t.is_nil() {
                    needs_ret_local = true;
                    rust_ty_to_binaryen(t)
                } else {
                    BinaryenNone()
                }
            },
            ty::FnOutput::FnDiverging => {
                BinaryenNone()
            }
        };

        let mut locals = Vec::new();

        for mir_local in &self.mir.var_decls {
            locals.push(rust_ty_to_binaryen(mir_local.ty));
        }

        for mir_local in &self.mir.temp_decls {
            locals.push(rust_ty_to_binaryen(mir_local.ty));
        }

        if needs_ret_local {
            locals.push(binaryen_ret);
        }

        // reelooper helper for irreducible control flow
        locals.push(BinaryenInt32());

        let relooper = unsafe { RelooperCreate() };

        let mut relooper_blocks = Vec::new();

        for bb in &self.mir.basic_blocks {
            let mut binaryen_stmts = Vec::new();
            for stmt in &bb.statements {
                match stmt.kind {
                    StatementKind::Assign(ref lval, ref rval) => {
                        let (lval_i, _) = self.trans_lval(lval);
                        if let Some(rval_expr) = self.trans_rval(rval) {
                            let binaryen_stmt = unsafe { BinaryenSetLocal(self.module, lval_i, rval_expr) };
                            binaryen_stmts.push(binaryen_stmt);
                        }
                    }
                }
            }
            match bb.terminator().kind {
                TerminatorKind::Return => {
                    let expr = self.trans_operand(&Operand::Consume(Lvalue::ReturnPointer));
                    let expr = unsafe { BinaryenReturn(self.module, expr) };
                    binaryen_stmts.push(expr);
                }
                _ => ()
            }
            unsafe {
                let binaryen_expr = BinaryenBlock(self.module, ptr::null(),
                                                  binaryen_stmts.as_ptr(),
                                                  BinaryenIndex(binaryen_stmts.len() as _));
                let relooper_block = RelooperAddBlock(relooper, binaryen_expr);
                relooper_blocks.push(relooper_block);
            }
        }

        for (i, bb) in self.mir.basic_blocks.iter().enumerate() {
            match bb.terminator().kind {
                TerminatorKind::Goto { ref target } => {
                    unsafe {
                        RelooperAddBranch(relooper_blocks[i], relooper_blocks[target.index()],
                                          BinaryenExpressionRef(ptr::null_mut()),
                                          BinaryenExpressionRef(ptr::null_mut()));
                    }
                }
                TerminatorKind::Return => {
                    /* handled during bb creation */
                }
                _ => panic!()
            }
        }

        unsafe {
            if !self.fun_types.contains_key(self.sig) {
                let name = format!("rustfn-{}", self.id);
                let name = CString::new(name).expect("");
                let name_ptr = name.as_ptr();
                self.c_strings.push(name);
                let ty = BinaryenAddFunctionType(self.module,
                                                 name_ptr,
                                                 binaryen_ret,
                                                 binaryen_args.as_ptr(),
                                                 BinaryenIndex(binaryen_args.len() as _));
                self.fun_types.insert(self.sig.clone(), ty);
            }

            let body = RelooperRenderAndDispose(relooper,
                                                relooper_blocks[0],
                                                BinaryenIndex(locals.len() as _),
                                                self.module);
            let name = sanitize_symbol(&self.tcx.node_path_str(self.id));
            let name = CString::new(name).expect("");
            let name_ptr = name.as_ptr();
            self.c_strings.push(name);
            BinaryenAddFunction(self.module, name_ptr,
                                *self.fun_types.get(self.sig).unwrap(),
                                locals.as_ptr(),
                                BinaryenIndex(locals.len() as _),
                                body);
        }
    }

    fn trans_lval(&mut self, lvalue: &Lvalue) -> (BinaryenIndex, u32) {
        let i = match *lvalue {
            Lvalue::Arg(i) => i,
            Lvalue::Var(i) => self.mir.arg_decls.len() as u32 + i,
            Lvalue::Temp(i) => {
                self.mir.arg_decls.len() as u32 +
                    self.mir.var_decls.len() as u32 + i
            }
            Lvalue::ReturnPointer => {
                self.mir.arg_decls.len() as u32 +
                    self.mir.var_decls.len() as u32 +
                    self.mir.temp_decls.len() as u32
            }
            _ => panic!()
        };

        (BinaryenIndex(i), 0)
    }

    fn trans_rval(&mut self, rvalue: &Rvalue<'tcx>) -> Option<BinaryenExpressionRef> {
        unsafe {
            match *rvalue {
                Rvalue::Use(ref operand) => {
                    Some(self.trans_operand(operand))
                }
                Rvalue::BinaryOp(ref op, ref a, ref b) => {
                    let a = self.trans_operand(a);
                    let b = self.trans_operand(b);
                    let op = match *op {
                        BinOp::Add => BinaryenAdd(),
                        _ => panic!()
                    };
                    Some(BinaryenBinary(self.module, op, a, b))
                }
                _ => {
                    None
                }
            }
        }
    }

    fn trans_operand(&mut self, operand: &Operand<'tcx>) -> BinaryenExpressionRef {
        match *operand {
            Operand::Consume(ref lvalue) => {
                let (i, _) = self.trans_lval(lvalue);
                let lval_ty = self.mir.lvalue_ty(self.tcx, lvalue);
                let t = lval_ty.to_ty(self.tcx);
                let t = rust_ty_to_binaryen(t);
                unsafe { BinaryenGetLocal(self.module, i, t) }
            }
            Operand::Constant(ref c) => {
                match c.literal {
                    Literal::Value { ref value } => {
                        match *value {
                            ConstVal::Integral(ConstInt::Isize(ConstIsize::Is32(val))) => {
                                unsafe {
                                    let lit = BinaryenLiteralInt32(val);
                                    BinaryenConst(self.module, lit)
                                }
                            }
                            _ => panic!()
                        }
                    }
                    _ => panic!()
                }
            }
        }
    }
}

fn rust_ty_to_binaryen<'tcx>(t: Ty<'tcx>) -> BinaryenType {
    // FIXME zero-sized-types
    match t.sty {
        ty::TyFloat(FloatTy::F32) => {
            BinaryenFloat32()
        },
        ty::TyFloat(FloatTy::F64) => {
            BinaryenFloat64()
        },
        ty::TyInt(IntTy::I64) | ty::TyUint(UintTy::U64) => {
            BinaryenInt64()
        }
        _ => {
            BinaryenInt32()
        }
    }
}

fn sanitize_symbol(s: &str) -> String {
    s.chars().map(|c| {
        match c {
            '<' | '>' | ' ' => '_',
            _ => c
        }
    }).collect()
}
