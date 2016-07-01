use libc::c_char;
use error::*;
use rustc::mir::repr::*;
use rustc::mir::mir_map::MirMap;
use rustc::middle::const_val::ConstVal;
use rustc_const_math::{ConstInt, ConstIsize};
use rustc::ty::{self, TyCtxt, Ty, FnSig};
use rustc::ty::layout::{self, Layout, Size};
use rustc::ty::subst::Substs;
use rustc::hir::intravisit::{self, Visitor, FnKind};
use rustc::hir::{FnDecl, Block};
use rustc::hir::def_id::DefId;
use rustc::traits::{ProjectionMode};
use syntax::ast::{NodeId, IntTy, UintTy, FloatTy};
use syntax::attr::AttrMetaMethods;
use syntax::codemap::Span;
use std::ffi::CString;
use std::ptr;
use std::collections::HashMap;
use binaryen::*;
use monomorphize;
use traits;

pub fn trans_crate<'a, 'tcx>(tcx: &TyCtxt<'a, 'tcx, 'tcx>,
                             mir_map: &MirMap<'tcx>) -> Result<()> {

    let _ignore = tcx.dep_graph.in_ignore();

    let ref mut v = BinaryenModuleCtxt {
        tcx: tcx,
        mir_map: mir_map,
        module: unsafe { BinaryenModuleCreate() },
        fun_types: HashMap::new(),
        fun_names: HashMap::new(),
        c_strings: Vec::new(),
    };

    tcx.map.krate().visit_all_items(v);

    unsafe {
        // TODO: validate the module
        // TODO: make it possible to print the optimized module
        BinaryenModulePrint(v.module);
    }

    Ok(())
}

struct BinaryenModuleCtxt<'v, 'tcx: 'v> {
    tcx: &'v TyCtxt<'v, 'tcx, 'tcx>,
    mir_map: &'v MirMap<'tcx>,
    module: BinaryenModuleRef,
    fun_types: HashMap<ty::FnSig<'tcx>, BinaryenFunctionTypeRef>,
    fun_names: HashMap<(DefId, ty::FnSig<'tcx>), CString>,
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
        let did = self.tcx.map.local_def_id(id);
        let type_scheme = self.tcx.lookup_item_type(did);

        // don't translate generic functions yet
        if !type_scheme.generics.types.is_empty() {
            return;
        }

        let mir = &self.mir_map.map[&id];
        let sig = type_scheme.ty.fn_sig().skip_binder();

        {
            let mut ctxt = BinaryenFnCtxt {
                tcx: self.tcx,
                mir_map: self.mir_map,
                mir: mir,
                did: did,
                sig: &sig,
                module: self.module,
                fun_types: &mut self.fun_types,
                fun_names: &mut self.fun_names,
                c_strings: &mut self.c_strings,
            };

            ctxt.trans();
        }

        intravisit::walk_fn(self, fk, fd, b, s)
    }
}

struct BinaryenFnCtxt<'v, 'tcx: 'v> {
    tcx: &'v TyCtxt<'v, 'tcx, 'tcx>,
    mir_map: &'v MirMap<'tcx>,
    mir: &'v Mir<'tcx>,
    did: DefId,
    sig: &'v FnSig<'tcx>,
    module: BinaryenModuleRef,
    fun_types: &'v mut HashMap<ty::FnSig<'tcx>, BinaryenFunctionTypeRef>,
    fun_names: &'v mut HashMap<(DefId, ty::FnSig<'tcx>), CString>,
    c_strings: &'v mut Vec<CString>,
}

#[derive(Debug)]
enum BinaryenCallKind {
    Direct,
    Import,
    // Indirect // unimplemented at the moment
}

impl<'v, 'tcx: 'v> BinaryenFnCtxt<'v, 'tcx> {
    /// This is the main entry point for MIR->wasm fn translation
    fn trans(&mut self) {

        // Maintain a cache of translated monomorphizations and bail
        // if we've already seen this one.
        let fn_name_ptr;
        if self.fun_names.contains_key(&(self.did, self.sig.clone())) {
            return;
        } else {
            let fn_name = sanitize_symbol(&self.tcx.item_path_str(self.did));
            let fn_name = CString::new(fn_name).expect("");
            fn_name_ptr = fn_name.as_ptr();
            self.fun_names.insert((self.did, self.sig.clone()), fn_name);
        }

        debug!("translating fn {:?}", self.tcx.item_path_str(self.did));

        // Translate arg and ret tys to wasm
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

        // Create the wasm locals
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

        // stack pointer
        locals.push(BinaryenInt32());
        let stack_pointer_local = BinaryenIndex(locals.len() as _);

        // reelooper helper for irreducible control flow
        locals.push(BinaryenInt32());
        let relooper_local = BinaryenIndex(locals.len() as _);

        debug!("{} wasm locals found", locals.len());

        // Create the relooper for tying together basic blocks. We're
        // going to first translate the basic blocks without the
        // terminators, then go back over the basic blocks and use the
        // terminators to configure the relooper.
        let relooper = unsafe { RelooperCreate() };

        let mut relooper_blocks = Vec::new();

        // Create the function prologue
        unsafe {
            let read_sp = BinaryenLoad(self.module, 4, 0, 0, 0, BinaryenInt32(), BinaryenConst(self.module, BinaryenLiteralInt32(0)));
            let copy_sp = BinaryenSetLocal(self.module, stack_pointer_local, read_sp);
            let relooper_block = RelooperAddBlock(relooper, copy_sp);
            relooper_blocks.push(relooper_block);
        }

        debug!("{} MIR basic blocks to translate", self.mir.basic_blocks.len());

        for (i, bb) in self.mir.basic_blocks.iter().enumerate() {
            debug!("bb{}: {:#?}", i, bb);

            let mut binaryen_stmts = Vec::new();
            for stmt in &bb.statements {
                match stmt.kind {
                    StatementKind::Assign(ref lvalue, ref rvalue) => {
                        self.trans_assignment(lvalue, rvalue, &mut binaryen_stmts);
                        // let (lvalue_idx, _) = self.trans_lval(lvalue);
                        // let dest_ty = self.mir.lvalue_ty(*self.tcx, lvalue).to_ty(*self.tcx);

                        // if let Some(rvalue_expr) = self.trans_rval(rvalue, &dest_ty) {
                        //     debug!("emitting SetLocal for Assign '{:?} = {:?}'", lvalue, rvalue);
                        //     let binaryen_stmt = unsafe { BinaryenSetLocal(self.module, lvalue_idx, rvalue_expr) };
                        //     binaryen_stmts.push(binaryen_stmt);
                        // }
                    }
                }
            }

            // Some features of MIR terminators tranlate to wasm
            // expressions, some translate to relooper edges. These
            // are the expressions.
            match bb.terminator().kind {
                TerminatorKind::Return => {
                    // Emit function epilogue:
                    unsafe {
                        let read_original_sp = BinaryenGetLocal(self.module, stack_pointer_local, BinaryenInt32());
                        let restore_original_sp = BinaryenStore(self.module, 4, 0, 0, BinaryenConst(self.module, BinaryenLiteralInt32(0)), read_original_sp);
                        binaryen_stmts.push(restore_original_sp);
                    }

                    debug!("emitting Return from fn {:?}", self.tcx.item_path_str(self.did));
                    let expr = self.trans_operand(&Operand::Consume(Lvalue::ReturnPointer));
                    let expr = unsafe { BinaryenReturn(self.module, expr) };
                    binaryen_stmts.push(expr);
                }
                TerminatorKind::Call {
                    ref func, ref args, ref destination, ..
                } => unsafe {
                    if let Some((b_func, b_fnty, call_kind)) = self.trans_fn_name_direct(func) {
                        let b_args: Vec<_> = args.iter().map(|a| self.trans_operand(a)).collect();
                        let b_call = match call_kind {
                            BinaryenCallKind::Direct => {
                                BinaryenCall(self.module,
                                             b_func,
                                             b_args.as_ptr(),
                                             BinaryenIndex(b_args.len() as _),
                                             b_fnty)
                            }
                            BinaryenCallKind::Import => {
                                BinaryenCallImport(self.module,
                                                   b_func,
                                                   b_args.as_ptr(),
                                                   BinaryenIndex(b_args.len() as _),
                                                   b_fnty)
                            }
                        };

                        match *destination {
                            Some((ref lvalue, _)) => {
                                // let b_lvalue = self.trans_lval(lvalue);

                                let (lvalue_index, lvalue_offset, lvalue_expr) = self.trans_lval(lvalue);
                                if lvalue_expr.is_some() {
                                    panic!("123");
                                }

                                if b_fnty == BinaryenNone() {
                                    // The result of the Rust call is put in MIR into a tmp local,
                                    // but the wasm function returns void (like the print externs)
                                    // TODO: what representation should the unit type have in wasm ?
                                    debug!("emitting {:?} Call to fn {:?} + SetLocal for unit type", call_kind, func);
                                    binaryen_stmts.push(b_call);

                                    let unit_type = BinaryenConst(self.module, BinaryenLiteralInt32(-1));
                                    binaryen_stmts.push(BinaryenSetLocal(self.module, lvalue_index, unit_type));
                                } else {
                                    debug!("emitting {:?} Call to fn {:?} + SetLocal of the result", call_kind, func);
                                    binaryen_stmts.push(BinaryenSetLocal(self.module, lvalue_index, b_call));
                                }
                            }
                            _ => {
                                debug!("emitting Call to fn {:?}", func);
                                binaryen_stmts.push(b_call);
                            }
                        }
                    } else {
                        panic!()
                    }
                },
                _ => ()
            }
            unsafe {
                let name = format!("bb{}", i);
                let name = CString::new(name).expect("");
                let name_ptr = name.as_ptr();
                self.c_strings.push(name);
                let binaryen_expr = BinaryenBlock(self.module, name_ptr,
                                                  binaryen_stmts.as_ptr(),
                                                  BinaryenIndex(binaryen_stmts.len() as _));
                let relooper_block = RelooperAddBlock(relooper, binaryen_expr);
                debug!("emitting {}-statement Block bb{}", binaryen_stmts.len(), i);
                relooper_blocks.push(relooper_block);
            }
        }

        // Create the relooper edges from the bb terminators
        for (i, bb) in self.mir.basic_blocks.iter().enumerate() {
            match bb.terminator().kind {
                TerminatorKind::Goto { ref target } => {
                    debug!("emitting Branch for Goto, from bb{} to bb{}", i, target.index());
                    unsafe {
                        RelooperAddBranch(relooper_blocks[i], relooper_blocks[target.index()],
                                          BinaryenExpressionRef(ptr::null_mut()),
                                          BinaryenExpressionRef(ptr::null_mut()));
                    }
                }
                TerminatorKind::If { ref cond, ref targets } => {
                    debug!("emitting Branches for If, from bb{} to bb{} and bb{}", i, targets.0.index(), targets.1.index());

                    let cond = self.trans_operand(cond);

                    unsafe {
                        RelooperAddBranch(relooper_blocks[i], relooper_blocks[targets.0.index()],
                                          cond,
                                          BinaryenExpressionRef(ptr::null_mut()));
                        RelooperAddBranch(relooper_blocks[i], relooper_blocks[targets.1.index()],
                                          BinaryenExpressionRef(ptr::null_mut()),
                                          BinaryenExpressionRef(ptr::null_mut()));
                    }
                }
                TerminatorKind::Return => {
                    /* handled during bb creation */
                }
                TerminatorKind::Call {
                    ref destination, ref cleanup, ..
                } => {
                    let _ = cleanup; // FIXME
                    match *destination {
                        Some((_, ref target)) => {
                            debug!("emitting Branch for Call, from bb{} to bb{}", i, target.index());
                            unsafe {
                                RelooperAddBranch(relooper_blocks[i], relooper_blocks[target.index()],
                                                  BinaryenExpressionRef(ptr::null_mut()),
                                                  BinaryenExpressionRef(ptr::null_mut()));
                            }
                        }
                        _ => panic!()
                    }
                }
                _ => panic!("unimplemented terminator {:?}", bb.terminator().kind)
            }
        }

        unsafe {
            if !self.fun_types.contains_key(self.sig) {
                let name = format!("rustfn-{}-{}", self.did.krate, self.did.index.as_u32());
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

            // Set up function prologue block
            if relooper_blocks.len() > 1 {
                RelooperAddBranch(relooper_blocks[0], relooper_blocks[1],
                                  BinaryenExpressionRef(ptr::null_mut()),
                                  BinaryenExpressionRef(ptr::null_mut()));
            }

            let body = RelooperRenderAndDispose(relooper,
                                                relooper_blocks[0],
                                                relooper_local,
                                                self.module);
            BinaryenAddFunction(self.module, fn_name_ptr,
                                *self.fun_types.get(self.sig).unwrap(),
                                locals.as_ptr(),
                                BinaryenIndex(locals.len() as _),
                                body);

            let nid = self.tcx.map.as_local_node_id(self.did).expect("");
            let attrs = self.tcx.map.attrs(nid);

            // let wasm_export_fn = attrs.iter().filter(|&attr| "wasm_export" == attr.node.value.name()).count() > 0;
            // if wasm_export_fn {
            //     debug!("emitting Export for fn {:?}", self.tcx.item_path_str(self.did));
            //     BinaryenAddExport(self.module, fn_name_ptr, fn_name_ptr);
            // }

            for attr in attrs {
                let name = attr.node.value.name().to_string();
                if name == "start" || name == "main" {
                    let wasm_start = self.generate_runtime_start(&name);
                    debug!("emitting wasm Start fn into fn {:?}", self.tcx.item_path_str(self.did));
                    BinaryenSetStart(self.module, wasm_start);
                }
            }

            // // TODO: there should be a compilation failure if more than one wasm start fn is found
            // let wasm_start_fn = attrs.iter().filter(|&attr| {
            //     let name = attr.node.value.name();
            //     name == "wasm_start" || name == "main"
            // }).count() > 0;

            // if wasm_start_fn {
            //     debug!("emitting SetStart for fn {:?}", self.tcx.item_path_str(self.did));
            //     BinaryenSetStart(self.module, f);
            // }
        }

        debug!("done translating fn {:?}\n", self.tcx.item_path_str(self.did));
    }

    fn trans_assignment(&mut self, lvalue: &Lvalue<'tcx>, rvalue: &Rvalue<'tcx>, statements: &mut Vec<BinaryenExpressionRef>) {
        // debug!("trans_assignment '{:?} = {:?}'", lvalue, rvalue);

        let (lvalue_index, lvalue_offset, lvalue_expr) = self.trans_lval(lvalue);
        let dest_ty = self.mir.lvalue_ty(*self.tcx, lvalue).to_ty(*self.tcx);

        // tmp
        if lvalue_expr.is_some() {
            panic!("trans_assignment lvalue_expr {:?}", lvalue_expr);
        }

        match *rvalue {
            Rvalue::Use(ref operand) => {
                let src = self.trans_operand(operand);

                unsafe {
                    let statement = match lvalue_offset {
                        Some(offset) => {
                            debug!("emitting Store + GetLocal for Assign Use '{:?} = {:?}'", lvalue, rvalue);
                            let ptr = BinaryenGetLocal(self.module, lvalue_index, rust_ty_to_binaryen(dest_ty));
                            // TODO: match the dest_ty to see how many bytes to write
                            BinaryenStore(self.module, 4, offset * 8, 0, ptr, src)
                        }
                        None => {
                            debug!("emitting SetLocal for Assign Use '{:?} = {:?}'", lvalue, rvalue);
                            BinaryenSetLocal(self.module, lvalue_index, src)
                        }                        
                    };
                    statements.push(statement);
                }
            }

            Rvalue::BinaryOp(ref op, ref left, ref right) => {
                let left = self.trans_operand(left);
                let right = self.trans_operand(right);

                unsafe {
                    // TODO: implement binary ops for other types than just i32s
                    let op = match *op {
                        BinOp::Add => BinaryenAddInt32(),
                        BinOp::Sub => BinaryenSubInt32(),
                        BinOp::Mul => BinaryenMulInt32(),
                        BinOp::Div => BinaryenDivSInt32(),
                        BinOp::Eq => BinaryenEqInt32(),
                        BinOp::Ne => BinaryenNeInt32(),
                        _ => panic!("unimplemented BinOp: {:?}", op)
                    };

                    debug!("emitting SetLocal for Assign BinaryOp '{:?} = {:?}'", lvalue, rvalue);
                    let op = BinaryenBinary(self.module, op, left, right);
                    statements.push(BinaryenSetLocal(self.module, lvalue_index, op));
                }
            }

            Rvalue::Ref( _, _, ref lvalue) => {
                // TODO: for shared refs only ?
                // tmp
                // panic!("Rvalue::Ref {:?}", lvalue);

                let expr = self.trans_operand(&Operand::Consume(lvalue.clone()));
                statements.push(expr);
            }

            Rvalue::Aggregate (ref kind, ref operands) => {
                match *kind {
                    AggregateKind::Adt(ref adt_def, _, ref substs) => {
                        let dest_layout = self.type_layout_with_substs(dest_ty, substs);

                        // TODO: check sizes, alignments (abi vs preferred), etc
                        let dest_size = self.type_size_with_substs(dest_ty, substs) as i32 * 8;

                        match *dest_layout {
                            Layout::Univariant { ref variant, .. } => {
                                // NOTE: use the variant's min_size and alignment for dest_size ?
                                unsafe {
                                    // alloca
                                    debug!("allocating struct {:?} in linear memory, size: {:?} bytes ", adt_def, dest_size);

                                    let sp = BinaryenConst(self.module, BinaryenLiteralInt32(0));
                                    let read_sp = BinaryenLoad(self.module, 4, 0, 0, 0, BinaryenInt32(), sp);

                                    let dest_size = BinaryenConst(self.module, BinaryenLiteralInt32(dest_size));
                                    let decr_sp = BinaryenBinary(self.module, BinaryenSubInt32(), read_sp, dest_size); 
                                    let write_sp = BinaryenStore(self.module, 4, 0, 0, sp, decr_sp);
                                    let write_local = BinaryenSetLocal(self.module, lvalue_index, write_sp);

                                    statements.push(write_local);

                                    // set fields
                                    let offsets = ::std::iter::once(0).chain(variant.offset_after_field.iter().map(|s| s.bytes()));
                                    for (offset, operand) in offsets.into_iter().zip(operands) {
                                        // let operand_ty = self.mir.operand_ty(*self.tcx, operand);
                                        // TODO: match on the operand_ty to know how much bytes to store

                                        let src = self.trans_operand(operand);
                                        let write_field = BinaryenStore(self.module, 4, offset as u32 * 8, 0, read_sp, src);
                                        debug!("emitting Store field, offset {:?}, value '{:?}'", offset * 8, operand);
                                        statements.push(write_field);                                
                                    }
                                }
                            }
                            _ => panic!("unimplemented Assign Aggregate Adt {:?} on Layout {:?}", adt_def, dest_layout)
                        }                        
                    }

                    AggregateKind::Tuple => {
                        if operands.len() == 0 {
                            debug!("ignoring Assign '{:?} = {:?}'", lvalue, rvalue);
                        } else {
                            panic!("unimplemented Assign '{:?} = {:?}'", lvalue, rvalue);
                        }
                    }

                    _ => panic!("unimplemented Assign Aggregate {:?}", kind)
                }
            }

            _ => panic!("unimplemented Assign '{:?} = {:?}'", lvalue, rvalue)
        }
    }

    fn trans_lval(&mut self, lvalue: &Lvalue<'tcx>) -> (BinaryenIndex, Option<u32>, Option<BinaryenExpressionRef>) {
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
            Lvalue::Projection(ref projection) => {
                let (base, offset, projection_expr) = self.trans_lval(&projection.base);
                let base_ty = self.mir.lvalue_ty(*self.tcx, &projection.base).to_ty(*self.tcx);

                // tmp
                // if projection_expr.is_some() {
                //     debug!("ELEM: {:?}", projection.elem);
                //     println!("----- EXPRESSION INCOMING");
                //     unsafe { BinaryenExpressionPrint(projection_expr.unwrap()); }
                //     println!("-----");
                //     panic!("trans_operand expr {:?}", projection_expr);
                // }

                // Those substs should probably be collected for the whole frame, like miri does
                let substs = self.tcx.mk_substs(Substs::empty());
                let base_layout = self.type_layout_with_substs(base_ty, substs);

                match projection.elem {
                    ProjectionElem::Deref => {
                        if offset.is_none() {
                            unsafe {
                                debug!("emitting GetLocal + Load to Deref '{:?}'", lvalue);

                                let ptr = BinaryenGetLocal(self.module, base, rust_ty_to_binaryen(base_ty));
                                // TODO: match the base_ty, and see how many bytes to read
                                let expr = BinaryenLoad(self.module, 4, 0, 0, 0, BinaryenInt32(), ptr);
                                // panic!("trans_lval DONE - DEREF {:?} - {:?}", lvalue, base.0);

                                if projection_expr.is_some() {
                                    println!("----- EXPRESSION INCOMING");
                                    BinaryenExpressionPrint(projection_expr.unwrap());
                                    println!("-----");
                                    BinaryenExpressionPrint(expr);
                                    println!("-----");
                                    panic!("");
                                }

                                return (base, None, Some(expr));
                            }                            
                        }
                        panic!("unimplemented Deref {:?}", lvalue);
                    }
                    ProjectionElem::Field(ref field, _) => {
                        let variant = match *base_layout {
                            Layout::Univariant { ref variant, .. } => variant,
                            _ => panic!("unimplemented Field Projection: {:?}", projection)
                        };
                        
                        if projection_expr.is_some() {
                            debug!("ELEM: {:?}", projection.elem);
                            // println!("----- EXPRESSION INCOMING");
                            // unsafe { BinaryenExpressionPrint(projection_expr.unwrap()); }
                            // println!("-----");
                            // panic!("");
                        }

                        let offset = variant.field_offset(field.index()).bytes();
                        // debug!("trans_lval DONE - DEREF {:?} OFFSET: {:?}", lvalue, offset);
                        return (base, Some(offset as u32), None);
                        // return (base, Some(offset as u32), projection_expr);
                    }
                    _ => panic!("unimplemented Projection: {:?}", projection)
                }
            }
            _ => panic!("unimplemented Lvalue: {:?}", lvalue)
        };

        // debug!("trans_lval DONE {:?}", i);
        (BinaryenIndex(i), None, None)
    }

    // fn trans_rval(&mut self, rvalue: &Rvalue<'tcx>, dest_ty: &'tcx TyS<'tcx>) -> Option<BinaryenExpressionRef> {
    //     unsafe {
    //         match *rvalue {
    //             Rvalue::Use(ref operand) => {
    //                 Some(self.trans_operand(operand))
    //             }
    //             Rvalue::BinaryOp(ref op, ref left, ref right) => {
    //                 let left = self.trans_operand(left);
    //                 let right = self.trans_operand(right);
    //                 // TODO: implement binary ops for other types than just i32s
    //                 let op = match *op {
    //                     BinOp::Add => BinaryenAddInt32(),
    //                     BinOp::Sub => BinaryenSubInt32(),
    //                     BinOp::Mul => BinaryenMulInt32(),
    //                     BinOp::Div => BinaryenDivSInt32(),
    //                     BinOp::Eq => BinaryenEqInt32(),
    //                     BinOp::Ne => BinaryenNeInt32(),
    //                     _ => panic!("unimplemented BinOp: {:?}", op)
    //                 };
    //                 Some(BinaryenBinary(self.module, op, left, right))
    //             }
    //             Rvalue::Ref( _, _, ref lvalue) => {
    //                 // TODO: for shared refs, similar to Operand::Consume and could be shared
    //                 debug!("Rvalue::Ref {:?}", lvalue);
    //                 let (i, _) = self.trans_lval(lvalue);
    //                 let lval_ty = self.mir.lvalue_ty(*self.tcx, lvalue);
    //                 let t = lval_ty.to_ty(*self.tcx);
    //                 let t = rust_ty_to_binaryen(t);
    //                 Some(BinaryenGetLocal(self.module, i, t))
    //             }
    //             _ => {
    //                 None
    //             }
    //         }
    //     }
    // }

    fn trans_operand(&mut self, operand: &Operand<'tcx>) -> BinaryenExpressionRef {
        // debug!("trans_operand: {:?}", operand);
        match *operand {
            Operand::Consume(ref lvalue) => {
                // debug!("Operand::Consume lvalue: {:?}", lvalue);
                let (index, offset, expr) = self.trans_lval(lvalue);
                let lval_ty = self.mir.lvalue_ty(*self.tcx, lvalue);
                let t = lval_ty.to_ty(*self.tcx);
                let t = rust_ty_to_binaryen(t);

                unsafe {
                    match offset {
                        Some(offset) => {
                            debug!("emitting GetLocal + Load for '{:?}'", lvalue);
                            // let ptr = if expr.is_none() { BinaryenGetLocal(self.module, index, t) } else { expr.unwrap() };
                            let ptr = BinaryenGetLocal(self.module, index, t);

                            // TODO: match the field ty, and see how many bytes to read
                            let x = BinaryenLoad(self.module, 4, 0, offset * 8, 0, BinaryenInt32(), ptr);

                            // tmp
                            if expr.is_some() {
                                println!("----- EXPRESSION INCOMING");
                                BinaryenExpressionPrint(expr.unwrap());
                                println!("-----");
                                BinaryenExpressionPrint(x);
                                panic!("");
                            }

                            x
                        }
                        None => {
                            debug!("emitting GetLocal for '{:?}'", lvalue);
                            let x = BinaryenGetLocal(self.module, index, t);

                            if expr.is_some() {
                                println!("----- EXPRESSION INCOMING");
                                BinaryenExpressionPrint(expr.unwrap());
                                println!("-----");
                                BinaryenExpressionPrint(x);
                                panic!("");
                            }

                            x
                        }
                    }
                }
            }
            Operand::Constant(ref c) => {
                match c.literal {
                    Literal::Value { ref value } => {
                        match *value {
                            ConstVal::Integral(ConstInt::Isize(ConstIsize::Is32(val))) |
                            ConstVal::Integral(ConstInt::I32(val)) => {
                                unsafe {
                                    let lit = BinaryenLiteralInt32(val);
                                    BinaryenConst(self.module, lit)
                                }
                            }
                            ConstVal::Bool (val) => {
                                let val = if val { 1 } else { 0 };
                                unsafe {
                                    let lit = BinaryenLiteralInt32(val);
                                    BinaryenConst(self.module, lit)
                                }
                            }
                            _ => panic!("unimplemented value: {:?}", value)
                        }
                    }
                    Literal::Promoted { .. } => {
                        panic!("unimplemented Promoted literal: {:?}", c)
                    }
                    _ => panic!("{:?}", c)
                }
            }
        }
    }

    // pub fn monomorphize(&self, ty: Ty<'tcx>, substs: &'tcx Substs<'tcx>) -> Ty<'tcx> {
    //     let substituted = ty.subst(*self.tcx, substs);
    //     self.tcx.normalize_associated_type(&substituted)
    // }

    // fn type_size(&self, ty: Ty<'tcx>) -> usize {
    //     let substs = self.tcx.mk_substs(subst::Substs::empty());
    //     self.type_size_with_substs(ty, substs)
    // }

    fn type_size_with_substs(&self, ty: Ty<'tcx>, substs: &'tcx Substs<'tcx>) -> usize {
        self.type_layout_with_substs(ty, substs).size(&self.tcx.data_layout).bytes() as usize
    }

    // fn type_layout(&self, ty: Ty<'tcx>) -> &'tcx Layout {
    //     let substs = self.tcx.mk_substs(subst::Substs::empty());
    //     self.type_layout_with_substs(ty, substs)
    // }

    fn type_layout_with_substs(&self, ty: Ty<'tcx>, substs: &'tcx Substs<'tcx>) -> &'tcx Layout {
        // TODO(solson): Is this inefficient? Needs investigation.
        // NOTE(lqd): slightly modified from miri
        let ty = monomorphize::apply_ty_substs(self.tcx, substs, ty);

        self.tcx.normalizing_infer_ctxt(ProjectionMode::Any).enter(|infcx| {
            // TODO(solson): Report this error properly.
            ty.layout(&infcx).unwrap()
        })
    }

    // fn lvalue_ty(&self, lvalue: &Lvalue<'tcx>) -> Ty<'tcx> {
    //     let substs = self.tcx.mk_substs(subst::Substs::empty());
    //     self.monomorphize(self.mir.lvalue_ty(*self.tcx, lvalue).to_ty(*self.tcx), substs)
    // }

    fn trans_fn_name_direct(&mut self, operand: &Operand<'tcx>) -> Option<(*const c_char, BinaryenType, BinaryenCallKind)> {
        match *operand {
            Operand::Constant(ref c) => {
                match c.literal {
                    Literal::Item { def_id, ref substs } => {
                        let ty = self.tcx.lookup_item_type(def_id).ty;
                        if ty.is_fn() {
                            assert!(def_id.is_local());
                            let sig = ty.fn_sig().skip_binder();

                            let mut fn_did = def_id;
                            let fn_name = self.tcx.item_path_str(fn_did);
                            let fn_sig;
                            let mut call_kind = BinaryenCallKind::Direct;

                            if fn_name == "wasm::::print_i32" || fn_name == "wasm::::_print_i32" {
                                // extern wasm functions
                                fn_sig = sig.clone();
                                call_kind = BinaryenCallKind::Import;
                                self.import_wasm_extern(fn_did, sig);
                            } else {
                                let nid = self.tcx.map.as_local_node_id(fn_did).expect("");
                                let (nid, substs, sig) = if self.mir_map.map.contains_key(&nid) {
                                    (nid, *substs, sig)
                                } else {
                                    // only trait methods can have a Self parameter
                                    if substs.self_ty().is_none() {
                                        panic!("unimplemented fn trans: {:?}", fn_did);
                                    }

                                    let (resolved_def_id, resolved_substs) = traits::resolve_trait_method(self.tcx, fn_did, substs);
                                    let nid = self.tcx.map.as_local_node_id(resolved_def_id).expect("");
                                    let ty = self.tcx.lookup_item_type(resolved_def_id).ty;
                                    let sig = ty.fn_sig().skip_binder();

                                    fn_did = resolved_def_id;
                                    (nid, resolved_substs, sig)
                                };

                                let mir = &self.mir_map.map[&nid];

                                fn_sig = monomorphize::apply_param_substs(self.tcx, substs, sig);
                                {
                                    let mut ctxt = BinaryenFnCtxt {
                                        tcx: self.tcx,
                                        mir_map: self.mir_map,
                                        mir: mir,
                                        did: fn_did,
                                        sig: &fn_sig,
                                        module: self.module,
                                        fun_types: &mut self.fun_types,
                                        fun_names: &mut self.fun_names,
                                        c_strings: &mut self.c_strings,
                                    };

                                    ctxt.trans();
                                }
                            }

                            let ret_ty = match fn_sig.output {
                                ty::FnOutput::FnConverging(ref t) => {
                                    if !t.is_nil() {
                                        rust_ty_to_binaryen(t)
                                    } else {
                                        BinaryenNone()
                                    }
                                }
                                _ => panic!()
                            };

                            Some((self.fun_names[&(fn_did, fn_sig)].as_ptr(), ret_ty, call_kind))
                        } else {
                            panic!();
                        }
                    }
                    _ => panic!("{:?}", c)
                }
            }
            _ => panic!()
        }
    }

    fn generate_runtime_start(&mut self, entry_fn: &str) -> BinaryenFunctionRef {
        // runtime start fn
        let runtime_start_name = "__wasm_start";
        let runtime_start_name = CString::new(runtime_start_name).expect("");
        let runtime_start_name_ptr = runtime_start_name.as_ptr();
        self.c_strings.push(runtime_start_name);

        let entry_fn_name = &self.fun_names[&(self.did, self.sig.clone())];

        unsafe {
            let runtime_start_ty = BinaryenAddFunctionType(self.module,
                                                           runtime_start_name_ptr,
                                                           BinaryenNone(),
                                                           ptr::null_mut(),
                                                           BinaryenIndex(0));

            let mut statements = vec![];

            // set-up memory and stack 
            // FIXME: decide how memory's going to work, the stack pointer address,
            //        track its initial size, etc
            //     -> temporarily, just ask for one page
            BinaryenSetMemory(self.module, BinaryenIndex(1), BinaryenIndex(1),
                              ptr::null(), ptr::null(), ptr::null(), ptr::null(),
                              BinaryenIndex(0));

            let stack_pointer = BinaryenConst(self.module, BinaryenLiteralInt32(0));
            let stack_top = BinaryenConst(self.module, BinaryenLiteralInt32(0xFFFF));
            let stack_init = BinaryenStore(self.module, 4, 0, 0, stack_pointer, stack_top);
            statements.push(stack_init);

            // call start_fn(0, 0) or main()
            let entry_fn_call;
            if entry_fn == "start" {
                let start_args = [
                    BinaryenConst(self.module, BinaryenLiteralInt32(0)),
                    BinaryenConst(self.module, BinaryenLiteralInt32(0))
                ];
                entry_fn_call = BinaryenCall(self.module,
                                             entry_fn_name.as_ptr(),
                                             start_args.as_ptr(),
                                             BinaryenIndex(start_args.len() as _),
                                             BinaryenInt32());
            } else {
                assert!(entry_fn == "main");
                entry_fn_call = BinaryenCall(self.module,
                                             entry_fn_name.as_ptr(),
                                             ptr::null(),
                                             BinaryenIndex(0),
                                             BinaryenNone());
            }

            statements.push(entry_fn_call);

            let body = BinaryenBlock(self.module,
                                     ptr::null(),
                                     statements.as_ptr(),
                                     BinaryenIndex(statements.len() as _));

            BinaryenAddFunction(self.module,
                                runtime_start_name_ptr,
                                runtime_start_ty,
                                ptr::null_mut(),
                                BinaryenIndex(0),
                                body)
        }       
    }

    fn import_wasm_extern(&mut self, did: DefId, sig: &ty::FnSig<'tcx>) {
        if self.fun_names.contains_key(&(did, sig.clone())) {
            return;
        }

        // import print i32
        let print_i32_name = CString::new("print_i32").expect("");
        let print_i32 = print_i32_name.as_ptr();
        self.fun_names.insert((did, sig.clone()), print_i32_name.clone());
        self.c_strings.push(print_i32_name);

        let spectest_module_name = CString::new("spectest").expect("");
        let spectest_module = spectest_module_name.as_ptr();
        self.c_strings.push(spectest_module_name);

        let print_fn_name = CString::new("print").expect("");
        let print_fn = print_fn_name.as_ptr();
        self.c_strings.push(print_fn_name);

        let print_i32_args = [BinaryenInt32()];
        unsafe {
            let print_i32_ty = BinaryenAddFunctionType(self.module,
                                                       print_i32,
                                                       BinaryenNone(),
                                                       print_i32_args.as_ptr(),
                                                       BinaryenIndex(1));
            BinaryenAddImport(self.module,
                              print_i32,
                              spectest_module,
                              print_fn,
                              print_i32_ty);
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

trait StructExt {
    fn field_offset(&self, index: usize) -> Size;
}

impl StructExt for layout::Struct {
    fn field_offset(&self, index: usize) -> Size {
        if index == 0 {
            Size::from_bytes(0)
        } else {
            self.offset_after_field[index - 1]
        }
    }
}