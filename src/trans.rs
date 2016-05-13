use error::*;
use rustc::mir::repr::{Mir, StatementKind, Lvalue, Rvalue};
use rustc::mir::mir_map::MirMap;
use rustc::ty::{self, TyCtxt, Ty, FnSig};
use rustc::hir;
use rustc::hir::intravisit::{self, Visitor, FnKind};
use rustc::hir::{FnDecl, Block};
use syntax::ast::{NodeId, IntTy, UintTy, FloatTy};
use syntax::codemap::Span;
use std::ffi::{CString, CStr};
use std::ptr;
use std::collections::HashMap;
use binaryen::*;

pub fn translate_crate<'tcx>(tcx: &TyCtxt<'tcx>,
                             mir_map: &MirMap<'tcx>) -> Result<()> {

    let _ignore = tcx.dep_graph.in_ignore();

    let ref mut v = HirVisitor {
        tcx: tcx,
        mir_map: mir_map,
        module: unsafe { BinaryenModuleCreate() },
        fun_types: HashMap::new(),
    };

    tcx.map.krate().visit_all_items(v);

    unsafe {
        BinaryenModulePrint(v.module);
        BinaryenModuleDispose(v.module);
    }

    Ok(())
}

struct HirVisitor<'v, 'tcx: 'v> {
    tcx: &'v TyCtxt<'tcx>,
    mir_map: &'v MirMap<'tcx>,
    module: BinaryenModuleRef,
    fun_types: HashMap<&'tcx ty::FnSig<'tcx>, BinaryenFunctionTypeRef>,
}

impl<'v, 'tcx> Visitor<'v> for HirVisitor<'v, 'tcx> {
    fn visit_fn(&mut self, fk: FnKind<'v>, fd: &'v FnDecl,
                b: &'v Block, s: Span, id: NodeId) {
        let item = self.tcx.map.expect_item(id);
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
            };

            ctxt.translate();
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
    fun_types: &'v mut HashMap<&'tcx ty::FnSig<'tcx>, BinaryenFunctionTypeRef>,
}

impl<'v, 'tcx: 'v> BinaryenFnCtxt<'v, 'tcx> {
    fn translate(&mut self) {

        let binaryen_args: Vec<_> = self.sig.inputs.iter().map(|t| rust_ty_to_binaryen(t)).collect();
        let mut needs_ret_local = false;
        let binaryen_ret = match self.sig.output {
            ty::FnOutput::FnConverging(t) => {
                if !t.is_nil() {
                    needs_ret_local = true;
                    rust_ty_to_binaryen(t)
                } else {
                    unsafe { BinaryenNone() }
                }
            },
            ty::FnOutput::FnDiverging => {
                unsafe { BinaryenNone() }
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

        let relooper = unsafe { RelooperCreate() };

        let mut relooper_blocks = Vec::new();

        for bb in &self.mir.basic_blocks {
            let mut binaryen_stmts = Vec::new();
            for stmt in &bb.statements {
                match stmt.kind {
                    StatementKind::Assign(ref lval, ref rval) => {
                        let (lval_i, _) = self.translate_lval(lval);
                        let rval_expr = self.translate_rval(rval);
                        let binaryen_stmt = unsafe { BinaryenSetLocal(self.module, lval_i, rval_expr) };
                        binaryen_stmts.push(binaryen_stmt);
                    }
                }
            }
            unsafe {
                let binaryen_expr = BinaryenBlock(self.module, ptr::null(),
                                                  binaryen_stmts.as_ptr(),
                                                  binaryen_stmts.len() as _);
                let relooper_block = RelooperAddBlock(relooper, binaryen_expr);
                relooper_blocks.push(relooper_block);
            }
        }

        unsafe {
            let return_type : BinaryenType;

            if !self.fun_types.contains_key(self.sig) {
                let name = self.sig.to_string();
                let name = CString::new(name).expect("").into_raw(); // FIXME
                let ty = BinaryenAddFunctionType(self.module,
                                                 name,
                                                 binaryen_ret,
                                                 binaryen_args.as_ptr(), binaryen_args.len() as _);
                self.fun_types.insert(self.sig, ty);
            }

            let name = self.tcx.node_path_str(self.id);
            let name = CString::new(name).expect("").into_raw(); // FIXME
            let body = RelooperRenderAndDispose(relooper,
                                                relooper_blocks[0],
                                                !0, // FIXME
                                                self.module);
            BinaryenAddFunction(self.module, name,
                                *self.fun_types.get(self.sig).unwrap(),
                                locals.as_ptr(), locals.len() as _,
                                body);
        }
    }

    fn translate_lval(&mut self, lvalue: &Lvalue) -> (u32, u32) {
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

        (i, 0)
    }

    fn translate_rval(&mut self, rvalue: &Rvalue) -> BinaryenExpressionRef {
        unsafe { BinaryenNop(self.module) }
    }
}

fn rust_ty_to_binaryen<'tcx>(t: Ty<'tcx>) -> BinaryenType {
    // FIXME zero-sized-types
    match t.sty {
        ty::TyFloat(FloatTy::F32) => {
            unsafe { BinaryenFloat32() }
        },
        ty::TyFloat(FloatTy::F64) => {
            unsafe { BinaryenFloat64() }
        },
        ty::TyInt(IntTy::I64) | ty::TyUint(UintTy::U64) => {
            unsafe { BinaryenInt64() }
        }
        _ => {
            unsafe { BinaryenInt32() }
        }
    }
}
