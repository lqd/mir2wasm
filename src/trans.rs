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
use rustc::traits::ProjectionMode;
use syntax::ast::{NodeId, IntTy, UintTy, FloatTy};
use syntax::codemap::Span;
use std::ffi::CString;
use std::ptr;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use binaryen::*;
use monomorphize;
use traits;

#[derive(Debug, Clone, Copy)]
pub struct WasmTransOptions {
    pub optimize: bool,
    pub interpret: bool,
    pub print: bool,
    trace: bool,
}

impl WasmTransOptions {
    pub fn new() -> WasmTransOptions {
        WasmTransOptions {
            optimize: false,
            interpret: false,
            print: true,
            trace: false,
        }
    }
}

pub fn trans_crate<'a, 'tcx>(tcx: &TyCtxt<'a, 'tcx, 'tcx>,
                             mir_map: &MirMap<'tcx>,
                             entry_fn: Option<NodeId>,
                             options: &WasmTransOptions) -> Result<()> {

    let _ignore = tcx.dep_graph.in_ignore();

    let ref mut v = BinaryenModuleCtxt {
        tcx: tcx,
        mir_map: mir_map,
        module: unsafe { BinaryenModuleCreate() },
        entry_fn: entry_fn,
        fun_types: HashMap::new(),
        fun_names: HashMap::new(),
        c_strings: Vec::new(),
    };

    if options.trace {
        unsafe { BinaryenSetAPITracing(true) }
    }

    tcx.map.krate().visit_all_items(v);

    unsafe {
        // TODO: check which of the Binaryen optimization passes we want aren't on by default here.
        //       eg, removing unused functions and imports, minification, etc
        if options.optimize {
            BinaryenModuleOptimize(v.module);
        }

        assert!(BinaryenModuleValidate(v.module) == 1, "Internal compiler error: invalid generated module");

        if options.trace {
            BinaryenSetAPITracing(false);
        }

        if options.print && !options.interpret {
            BinaryenModulePrint(v.module);
        }

        if options.interpret {
            BinaryenModuleInterpret(v.module);
        }

        // TODO: add CLI option to save the module to a specified file: -o ?
        // BinaryenModuleWrite(v.module, ...)
    }

    Ok(())
}

struct BinaryenModuleCtxt<'v, 'tcx: 'v> {
    tcx: &'v TyCtxt<'v, 'tcx, 'tcx>,
    mir_map: &'v MirMap<'tcx>,
    module: BinaryenModuleRef,
    entry_fn: Option<NodeId>,
    fun_types: HashMap<ty::FnSig<'tcx>, BinaryenFunctionTypeRef>,
    fun_names: HashMap<(DefId, ty::FnSig<'tcx>), CString>,
    c_strings: Vec<CString>,
}

impl<'v, 'tcx: 'v> Drop for BinaryenModuleCtxt<'v, 'tcx> {
    fn drop(&mut self) {
        unsafe { BinaryenModuleDispose(self.module) };
    }
}

// The address in wasm linear memory where we store the stack pointer
// TODO: investigate where should the preferred location be
const STACK_POINTER_ADDRESS : i32 = 0;

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
                entry_fn: self.entry_fn,
                fun_types: &mut self.fun_types,
                fun_names: &mut self.fun_names,
                c_strings: &mut self.c_strings,
                frame: Frame::new(),
            };

            ctxt.trans();
        }

        intravisit::walk_fn(self, fk, fd, b, s)
    }
}

#[derive(Debug, PartialEq)]
enum FrameKind {
    Default,
    ReturnsToStack,
}

struct Frame {
    allocated: bool,
}

impl Frame {
    fn new() -> Frame {
        Frame {
            allocated: false,
        }
    }
}

struct BinaryenFnCtxt<'v, 'tcx: 'v> {
    tcx: &'v TyCtxt<'v, 'tcx, 'tcx>,
    mir_map: &'v MirMap<'tcx>,
    mir: &'v Mir<'tcx>,
    did: DefId,
    sig: &'v FnSig<'tcx>,
    module: BinaryenModuleRef,
    entry_fn: Option<NodeId>,
    fun_types: &'v mut HashMap<ty::FnSig<'tcx>, BinaryenFunctionTypeRef>,
    fun_names: &'v mut HashMap<(DefId, ty::FnSig<'tcx>), CString>,
    c_strings: &'v mut Vec<CString>,
    frame: Frame,
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

        let mut frame_kind = FrameKind::Default;
        let mut rust_ret_ty = None;
        let binaryen_ret_ty = match self.sig.output {
            ty::FnOutput::FnConverging(ty) => {
                rust_ret_ty = Some(ty);

                match ty.sty {
                    ty::TyEnum(..) | ty::TyStruct(..) => {
                        frame_kind = FrameKind::ReturnsToStack;
                    }
                    _ => ()
                }

                rust_ty_to_binaryen(ty)
            }

            ty::FnOutput::FnDiverging => {
                BinaryenNone()
            }
        };

        debug!("fn - ret_ty {:?}, frame_kind: {:?}", rust_ret_ty, frame_kind);

        // Create the wasm vars.
        // Params and vars form the list of locals, both sharing the same index space.
        let mut vars = Vec::new();

        for mir_var in &self.mir.var_decls {
            vars.push(rust_ty_to_binaryen(mir_var.ty));
        }

        for mir_var in &self.mir.temp_decls {
            vars.push(rust_ty_to_binaryen(mir_var.ty));
        }

        if rust_ret_ty.is_some() {
            vars.push(binaryen_ret_ty);
        }

        // Function prologue: frame pointer local
        vars.push(BinaryenInt32());
        let frame_pointer_local = BinaryenIndex((binaryen_args.len() + vars.len() - 1) as u32);

        // relooper helper local for irreducible control flow
        vars.push(BinaryenInt32());
        let relooper_local = BinaryenIndex((binaryen_args.len() + vars.len() - 1) as u32);

        let locals_count = binaryen_args.len() + vars.len();
        debug!("{} wasm locals initially found - params: {}, vars: {} (incl. frame pointer ${}, relooper helper ${})",
            locals_count, binaryen_args.len(), vars.len(), frame_pointer_local.0, relooper_local.0);

        // Create the relooper for tying together basic blocks. We're
        // going to first translate the basic blocks without the
        // terminators, then go back over the basic blocks and use the
        // terminators to configure the relooper.
        let relooper = unsafe { RelooperCreate() };

        let mut relooper_blocks = Vec::new();

        debug!("{} MIR basic blocks to translate", self.mir.basic_blocks.len());

        for (i, bb) in self.mir.basic_blocks.iter().enumerate() {
            debug!("bb{}: {:#?}", i, bb);

            let mut binaryen_stmts = Vec::new();
            for stmt in &bb.statements {
                match stmt.kind {
                    StatementKind::Assign(ref lvalue, ref rvalue) => {
                        self.trans_assignment(lvalue, rvalue, &mut binaryen_stmts);
                    }
                }
            }

            let mut block_kind = BinaryenBlockKind::Default;

            // Some features of MIR terminators tranlate to wasm
            // expressions, some translate to relooper edges. These
            // are the expressions.
            match bb.terminator().kind {
                TerminatorKind::Return => {
                    // Emit function epilogue:
                    unsafe {
                        if self.frame.allocated {
                            debug!("emitting function epilogue, GetLocal({}) + Store", frame_pointer_local.0);
                            let read_frame_pointer = BinaryenGetLocal(self.module, frame_pointer_local, BinaryenInt32());
                            let restore_stack_pointer = BinaryenStore(self.module, 4, 0, 0, self.emit_sp(), read_frame_pointer);
                            binaryen_stmts.push(restore_stack_pointer);
                        }
                    }

                    match frame_kind {
                        FrameKind::Default => {
                            debug!("emitting Return from fn {:?}", self.tcx.item_path_str(self.did));
                            let expr = self.trans_operand(&Operand::Consume(Lvalue::ReturnPointer));
                            let expr = unsafe { BinaryenReturn(self.module, expr) };
                            binaryen_stmts.push(expr);
                        },
                        FrameKind::ReturnsToStack => {
                            unsafe {
                                // let src = self.trans_operand(&Operand::Consume(Lvalue::ReturnPointer));
                                // // BinaryenExpressionPrint(src);

                                // TMP - the poor man's memcpy
                                // let dest = BinaryenGetLocal(self.module, frame_pointer_local, BinaryenInt32());
                                // let dest_size = self.type_size(rust_ret_ty.expect("")) as i32 * 8;

                                // debug!("tmp - emitting Stores to copy {}-byte result to frame pointer", dest_size);
                                // let mut bytes_to_copy = dest_size;
                                // let mut offset = 0;
                                // while bytes_to_copy > 0 {
                                //     let size = if bytes_to_copy >= 64 {
                                //         8
                                //     } else if bytes_to_copy >= 32 {
                                //         4
                                //     } else if bytes_to_copy >= 16 {
                                //         2
                                //     } else {
                                //         1
                                //     };

                                //     let ty = if size == 8 {
                                //         BinaryenInt64()
                                //     } else {
                                //         BinaryenInt32()
                                //     };

                                //     debug!("tmp - emitting Store copy, size: {}, offset: {}", size, offset);
                                //     let read_bytes = BinaryenLoad(self.module, size, 0, offset, 0, ty, src);
                                //     let copy_bytes = BinaryenStore(self.module, size, offset, 0, dest, read_bytes);
                                //     binaryen_stmts.push(copy_bytes);

                                //     // BinaryenExpressionPrint(copy_bytes);

                                //     bytes_to_copy -= size as i32 * 8;
                                //     offset += size;
                                // }

                                let read_frame_pointer = BinaryenGetLocal(self.module, frame_pointer_local, BinaryenInt32());

                                // copy the frame pointer value to the expected return var 
                                let ret_val = self.trans_lval(&Lvalue::ReturnPointer);
                                binaryen_stmts.push(BinaryenSetLocal(self.module, ret_val.index, read_frame_pointer));

                                // return the value (TODO: return the frame pointer directly and remove this local ?)
                                debug!("emitting Return frame pointer + SetLocal({}) from fn {:?}", ret_val.index.0, self.tcx.item_path_str(self.did));
                                let expr = BinaryenReturn(self.module, read_frame_pointer);
                                binaryen_stmts.push(expr);
                            }
                        }
                    }

                    // debug!("emitting Return from fn {:?}", self.tcx.item_path_str(self.did));
                    // let expr = self.trans_operand(&Operand::Consume(Lvalue::ReturnPointer));
                    // let expr = unsafe { BinaryenReturn(self.module, expr) };
                    // binaryen_stmts.push(expr);
                }
                TerminatorKind::Switch { ref discr, .. } => {
                    let adt = self.trans_lval(discr);
                    let adt_ty = self.mir.lvalue_ty(*self.tcx, discr).to_ty(*self.tcx);

                    if adt.offset.is_some() {
                        panic!("unimplemented Switch with offset");
                    }

                    let adt_layout = self.type_layout(adt_ty);
                    let discr_val = match *adt_layout {
                        Layout::General { discr, .. } => {
                            let discr_size = discr.size().bytes() as u32;
                            debug!("emitting GetLocal({}) + Load for ADT Switch condition", adt.index.0);
                            unsafe {
                                let ptr = BinaryenGetLocal(self.module, adt.index, BinaryenInt32());
                                BinaryenLoad(self.module, discr_size, 0, 0, 0, BinaryenInt32(), ptr)
                            }
                        }
                        Layout::CEnum { .. } => {
                            debug!("emitting GetLocal({}) for CEnum Switch condition", adt.index.0);
                            unsafe {
                                BinaryenGetLocal(self.module, adt.index, BinaryenInt32())
                            }
                        }
                        _ => panic!("unimplemented discrimant value for Layout {:?}", adt_layout)
                    };

                    block_kind = BinaryenBlockKind::Switch(discr_val);
                }
                TerminatorKind::Call {
                    ref func, ref args, ref destination, ..
                } => unsafe {
                    // NOTE: plan for the calling convention: i32/i64 f32/f64 are to be passed using the wasm stack and function parameters.
                    //       For the other types, the manual stack in linear memory will be used, and pointers into this stack
                    //       passed as i32s. A call to a function returning a struct will require preparing the output return value space on
                    //       the caller function's frame, and the called function will write its return value there to avoid memcpys

                    if let Some((ref lvalue, _)) = *destination {
                        let dest = self.trans_lval(lvalue);
                        let dest_ty = self.mir.lvalue_ty(*self.tcx, lvalue).to_ty(*self.tcx);

                        let (call, call_kind) = self.trans_fn_call(func, args);

                        if dest_ty.is_nil() {
                            // The result of the Rust call is put in MIR into a tmp local,
                            // but the wasm function returns void (like the print externs)
                            // TODO: the unit type should be represented as void in wasm, and its use
                            //       in locals and return values ignored for the module to validate
                            debug!("emitting {:?} Call to fn {:?} + SetLocal({}) for unit type", call_kind, func, dest.index.0);
                            binaryen_stmts.push(call);

                            let unit_type = BinaryenConst(self.module, BinaryenLiteralInt32(-1));
                            binaryen_stmts.push(BinaryenSetLocal(self.module, dest.index, unit_type));
                        } else {
                            let dest_layout = self.type_layout(dest_ty);
                            match *dest_layout {
                                Layout::Univariant { .. } | Layout::General { .. } => {
                                    let dest_size = self.type_size(dest_ty) as i32 * 8;

                                    debug!("allocating return value in linear memory to SetLocal({}), size: {:?}", dest.index.0, dest_size);
                                    let allocation = self.emit_alloca(dest.index, dest_size);
                                    binaryen_stmts.push(allocation);

                                    debug!("emitting {:?} Call to fn {:?}", call_kind, func);
                                    binaryen_stmts.push(call);
                                }

                                Layout::Scalar { .. } | Layout::CEnum { .. } => {
                                    debug!("emitting {:?} Call to fn {:?} + SetLocal({}) of the result", call_kind, func, dest.index.0);
                                    binaryen_stmts.push(BinaryenSetLocal(self.module, dest.index, call));
                                }

                                _ => panic!("unimplemented Call returned to Layout {:?}", dest_layout)
                            }
                        }
                    } else {
                        panic!("destination-less call");
                    }

                    // if let Some((b_func, b_fnty, call_kind)) = self.trans_fn_name_direct(func) {
                    //     let b_args: Vec<_> = args.iter().map(|a| self.trans_operand(a)).collect();
                    //     let b_call = match call_kind {
                    //         BinaryenCallKind::Direct => {
                    //             BinaryenCall(self.module,
                    //                          b_func,
                    //                          b_args.as_ptr(),
                    //                          BinaryenIndex(b_args.len() as _),
                    //                          b_fnty)
                    //         }
                    //         BinaryenCallKind::Import => {
                    //             BinaryenCallImport(self.module,
                    //                                b_func,
                    //                                b_args.as_ptr(),
                    //                                BinaryenIndex(b_args.len() as _),
                    //                                b_fnty)
                    //         }
                    //     };
                    //
                    //     match *destination {
                    //         Some((ref lvalue, _)) => {
                    //             let dest = self.trans_lval(lvalue);
                    //
                    //             if b_fnty == BinaryenNone() {
                    //                 // The result of the Rust call is put in MIR into a tmp local,
                    //                 // but the wasm function returns void (like the print externs)
                    //                 // TODO: what representation should the unit type have in wasm ?
                    //                 debug!("emitting {:?} Call to fn {:?} + SetLocal({}) for unit type", call_kind, func, dest.index.0);
                    //                 binaryen_stmts.push(b_call);
                    //
                    //                 let unit_type = BinaryenConst(self.module, BinaryenLiteralInt32(-1));
                    //                 binaryen_stmts.push(BinaryenSetLocal(self.module, dest.index, unit_type));
                    //             } else {
                    //                 let dest_ty = self.mir.lvalue_ty(*self.tcx, lvalue).to_ty(*self.tcx);
                    //                 let dest_layout = self.type_layout(dest_ty);
                    //
                    //                 match *dest_layout {
                    //                     Layout::Univariant { .. } | Layout::General { .. } => {
                    //                         // TODO: implement the calling convention for functions returning non-primitive types
                    //                         // FIXME: until then, emit byte copies, which is inefficient but works for now
                    //
                    //                         let dest_size = self.type_size(dest_ty) as i32 * 8;
                    //
                    //                         vars.push(BinaryenInt32());
                    //                         let tmp_dest = BinaryenIndex((binaryen_args.len() + vars.len() - 1) as u32);
                    //
                    //                         debug!("tmp - emitting {:?} Call to fn {:?} + SetLocal({}) of the result pointer", call_kind, func, tmp_dest.0);
                    //                         binaryen_stmts.push(BinaryenSetLocal(self.module, tmp_dest, b_call));
                    //
                    //                         debug!("tmp - allocating return value in linear memory to SetLocal({}), size: {:?}", dest.index.0, dest_size);
                    //                         let allocation = self.emit_alloca(dest.index, dest_size);
                    //                         binaryen_stmts.push(allocation);
                    //
                    //                         // TMP - the poor man's memcpy
                    //                         debug!("tmp - emitting Stores to copy result to stack frame");
                    //                         let ptr = BinaryenGetLocal(self.module, tmp_dest, BinaryenInt32());
                    //                         let sp = self.emit_read_sp();
                    //                         let mut bytes_to_copy = dest_size;
                    //                         let mut offset = 0;
                    //                         while bytes_to_copy > 0 {
                    //                             let size = if bytes_to_copy >= 64 {
                    //                                 8
                    //                             } else if bytes_to_copy >= 32 {
                    //                                 4
                    //                             } else if bytes_to_copy >= 16 {
                    //                                 2
                    //                             } else {
                    //                                 1
                    //                             };
                    //
                    //                             let ty = if size == 8 {
                    //                                 BinaryenInt64()
                    //                             } else {
                    //                                 BinaryenInt32()
                    //                             };
                    //
                    //                             debug!("tmp - emitting Store copy, size: {}, offset: {}", size, offset);
                    //                             let read_bytes = BinaryenLoad(self.module, size, 0, offset, 0, ty, ptr);
                    //                             let copy_bytes = BinaryenStore(self.module, size, offset, 0, sp, read_bytes);
                    //                             binaryen_stmts.push(copy_bytes);
                    //
                    //                             bytes_to_copy -= size as i32 * 8;
                    //                             offset += size;
                    //                         }
                    //                     }
                    //
                    //                     Layout::Scalar { .. } | Layout::CEnum { .. } => {
                    //                         debug!("emitting {:?} Call to fn {:?} + SetLocal({}) of the result", call_kind, func, dest.index.0);
                    //                         binaryen_stmts.push(BinaryenSetLocal(self.module, dest.index, b_call));
                    //                     }
                    //
                    //                     _ => panic!("unimplemented Call returned to Layout {:?}", dest_layout)
                    //                 }
                    //             }
                    //         }
                    //         _ => {
                    //             debug!("emitting Call to fn {:?}", func);
                    //             binaryen_stmts.push(b_call);
                    //         }
                    //     }
                    // } else {
                    //     panic!("untranslated fn call to {:?}", func)
                    // }
                },
                _ => ()
            }
            unsafe {
                let name = format!("bb{}", i);
                let name = CString::new(name).expect("");
                let name_ptr = name.as_ptr();
                self.c_strings.push(name);

                debug!("emitting {}-statement Block bb{}", binaryen_stmts.len(), i);
                let binaryen_expr = BinaryenBlock(self.module, name_ptr,
                                                  binaryen_stmts.as_ptr(),
                                                  BinaryenIndex(binaryen_stmts.len() as _));
                let relooper_block = match block_kind {
                    BinaryenBlockKind::Default => RelooperAddBlock(relooper, binaryen_expr),
                    BinaryenBlockKind::Switch(ref cond) => RelooperAddBlockWithSwitch(relooper, binaryen_expr, *cond)
                };
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
                TerminatorKind::Switch { ref adt_def, ref targets, .. } => {
                    // We're required to have only unique (from, to) edges, while we have
                    // a variant to target mapping, where multiple variants can branch to
                    // the same target block. So group them by target block index.
                    let target_per_variant = targets.iter().map(|&t| t.index());
                    let mut variants_per_target = HashMap::new();
                    for (variant, target) in target_per_variant.enumerate() {
                        match variants_per_target.entry(target) {
                    		Entry::Vacant(entry) => { entry.insert(vec![variant]); },
                    		Entry::Occupied(mut entry) => { entry.get_mut().push(variant); },
                    	}
                    }

                    for (target, variants) in variants_per_target {
                        debug!("emitting Switch branch from bb{} to bb{}, for Enum '{:?}' variants {:?}", i, target, adt_def, variants);

                        // TODO: is it necessary to handle cases where the discriminant is not a valid u32 ? (doubtful)
                        let labels = variants.iter().map(|&v| {
                            let discr_val = adt_def.variants[v].disr_val.to_u64_unchecked() as u32;
                            BinaryenIndex(discr_val)
                        }).collect::<Vec<_>>();

                        // wasm also requires to have a "default" branch, even though this is less useful to us
                        // as we have a target for every variant.
                        // TODO: figure out the best way to handle this, maybe add an unreachable block to trigger
                        //       an error. In the meantime, consider the edge to the first variant as the default branch.
                        //       And apparently the LLVM backend emits a random branch as the default one.
                        let (labels_ptr, labels_count) = if variants.contains(&0) {
                            (ptr::null(), 0)
                        } else {
                            (labels.as_ptr(), labels.len())
                        };

                        unsafe {
                            RelooperAddBranchForSwitch(relooper_blocks[i], relooper_blocks[target],
                                                       labels_ptr, BinaryenIndex(labels_count as _),
                                                       BinaryenExpressionRef(ptr::null_mut()));
                        }
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
                                                 binaryen_ret_ty,
                                                 binaryen_args.as_ptr(),
                                                 BinaryenIndex(binaryen_args.len() as _));
                self.fun_types.insert(self.sig.clone(), ty);
            }

            let mut first_block = relooper_blocks[0];
            if self.frame.allocated || frame_kind == FrameKind::ReturnsToStack {
                // Create the function prologue
                debug!("emitting function prologue, SetLocal({}) + Load", frame_pointer_local.0);
                let prepare_frame_pointer = BinaryenSetLocal(self.module, frame_pointer_local, self.emit_read_sp());
                let prologue = RelooperAddBlock(relooper, prepare_frame_pointer);

                RelooperAddBranch(prologue, first_block,
                                  BinaryenExpressionRef(ptr::null_mut()),
                                  BinaryenExpressionRef(ptr::null_mut()));

                first_block = prologue;
            }

            let body = RelooperRenderAndDispose(relooper,
                                                first_block,
                                                relooper_local,
                                                self.module);

            BinaryenAddFunction(self.module, fn_name_ptr,
                                *self.fun_types.get(self.sig).unwrap(),
                                vars.as_ptr(),
                                BinaryenIndex(vars.len() as _),
                                body);

            let nid = self.tcx.map.as_local_node_id(self.did).expect("");
            let is_entry_fn = match self.entry_fn {
                Some(node_id) => node_id == nid,
                None => false,
            };

            if is_entry_fn {
                let is_start = self.mir.arg_decls.len() == 2;
                let entry_fn_name = if is_start { "start" } else { "main" };
                let wasm_start = self.generate_runtime_start(&entry_fn_name);
                debug!("emitting wasm Start fn into entry_fn {:?}", self.tcx.item_path_str(self.did));
                BinaryenSetStart(self.module, wasm_start);
            }
        }

        debug!("done translating fn {:?}\n", self.tcx.item_path_str(self.did));
    }

    fn trans_assignment(&mut self, lvalue: &Lvalue<'tcx>, rvalue: &Rvalue<'tcx>, statements: &mut Vec<BinaryenExpressionRef>) {
        let dest = self.trans_lval(lvalue);
        let dest_ty = self.mir.lvalue_ty(*self.tcx, lvalue).to_ty(*self.tcx);

        let dest_layout = self.type_layout(dest_ty);

        match *rvalue {
            Rvalue::Use(ref operand) => {
                let src = self.trans_operand(operand);
                unsafe {
                    let statement = match dest.offset {
                        Some(offset) => {
                            debug!("emitting Store + GetLocal({}) for Assign Use '{:?} = {:?}'", dest.index.0, lvalue, rvalue);
                            let ptr = BinaryenGetLocal(self.module, dest.index, rust_ty_to_binaryen(dest_ty));
                            // TODO: match on the dest_ty to know how many bytes to write, not just i32s
                            BinaryenStore(self.module, 4, offset, 0, ptr, src)
                        }
                        None => {
                            match *dest_layout {
                                Layout::Scalar { .. } => {
                                    // TODO: handle Pointer Scalars differently ?
                                    debug!("emitting SetLocal({}) for Assign Use Scalar '{:?} = {:?}'", dest.index.0, lvalue, rvalue);
                                    BinaryenSetLocal(self.module, dest.index, src)
                                }

                                _ => panic!("DEST TY {:?}, LAYOUT !!!: {:?}", dest_ty, dest_layout)
                            }                           
                        }
                    };
                    statements.push(statement);
                }
            }

            Rvalue::BinaryOp(ref op, ref left, ref right) => {
                let left = self.trans_operand(left);
                let right = self.trans_operand(right);

                unsafe {
                    // TODO: match on dest_ty.sty to implement binary ops for other types than just i32s
                    // TODO: check if the dest_layout is signed or not (CEnum, etc)
                    // TODO: comparisons are signed only for now, so implement unsigned ones
                    let op = match *op {
                        BinOp::Add => BinaryenAddInt32(),
                        BinOp::Sub => BinaryenSubInt32(),
                        BinOp::Mul => BinaryenMulInt32(),
                        BinOp::Div => BinaryenDivSInt32(),
                        BinOp::Eq => BinaryenEqInt32(),
                        BinOp::Ne => BinaryenNeInt32(),
                        BinOp::Lt => BinaryenLtSInt32(),
                        BinOp::Le => BinaryenLeSInt32(),
                        BinOp::Gt => BinaryenGtSInt32(),
                        BinOp::Ge => BinaryenGeSInt32(),
                        _ => panic!("unimplemented BinOp: {:?}", op)
                    };

                    let op = BinaryenBinary(self.module, op, left, right);
                    let statement = match dest.offset {
                        Some(offset) => {
                            debug!("emitting Store + GetLocal({}) for Assign BinaryOp '{:?} = {:?}'", dest.index.0, lvalue, rvalue);
                            let ptr = BinaryenGetLocal(self.module, dest.index, rust_ty_to_binaryen(dest_ty));
                            // TODO: match on the dest_ty to know how many bytes to write, not just i32s
                            BinaryenStore(self.module, 4, offset, 0, ptr, op)
                        }
                        None => {
                            debug!("emitting SetLocal({}) for Assign BinaryOp '{:?} = {:?}'", dest.index.0, lvalue, rvalue);
                            BinaryenSetLocal(self.module, dest.index, op)
                        }
                    };
                    statements.push(statement);
                }
            }

            Rvalue::Ref( _, _, ref lvalue) => {
                // TODO: for shared refs only ?
                // TODO: works for refs to "our stack", but not the locals on the wasm stack yet
                let expr = self.trans_operand(&Operand::Consume(lvalue.clone()));
                unsafe {
                    debug!("emitting SetLocal({}) for Assign Ref '{:?} = {:?}'", dest.index.0, lvalue, rvalue);
                    let expr = BinaryenSetLocal(self.module, dest.index, expr);
                    statements.push(expr);
                }
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
                                debug!("allocating struct '{:?}' in linear memory to SetLocal({}), size: {:?} bytes ", adt_def, dest.index.0, dest_size);
                                let allocation = self.emit_alloca(dest.index, dest_size);
                                statements.push(allocation);

                                let offsets = ::std::iter::once(0).chain(variant.offset_after_field.iter().map(|s| s.bytes()));
                                debug!("emitting Stores for struct '{:?}' fields, values: {:?}", adt_def, operands);
                                self.emit_assign_fields(offsets, operands, statements);
                            }

                            Layout::General { discr, ref variants, .. } => {
                                if let AggregateKind::Adt(ref adt_def, variant, _) = *kind {
                                    let discr_val = adt_def.variants[variant].disr_val.to_u32().unwrap();
                                    let discr_size = discr.size().bytes() as u32;

                                    debug!("allocating Enum '{:?}' in linear memory to SetLocal({}), size: {:?} bytes", adt_def, dest.index.0, dest_size);
                                    let allocation = self.emit_alloca(dest.index, dest_size);
                                    statements.push(allocation);

                                    // set enum discr
                                    unsafe {
                                        debug!("emitting Store for Enum '{:?}' discr: {:?}", adt_def, discr_val);
                                        let discr_val = BinaryenConst(self.module, BinaryenLiteralInt32(discr_val as i32));
                                        let write_discr = BinaryenStore(self.module, discr_size, 0, 0, self.emit_read_sp(), discr_val);
                                        statements.push(write_discr);
                                    }

                                    debug!("emitting Stores for Enum '{:?}' fields, operands '{:?}'", adt_def, operands);
                                    let offsets = variants[variant].offset_after_field.iter().map(|s| s.bytes());
                                    self.emit_assign_fields(offsets, operands, statements);
                                } else {
                                    panic!("tried to assign {:?} to Layout::General", kind);
                                }
                            }

                            Layout::CEnum { discr, .. } => {
                                assert_eq!(operands.len(), 0);
                                if let AggregateKind::Adt(adt_def, variant, _) = *kind {
                                    let discr_size = discr.size().bytes();
                                    if discr_size > 4 {
                                        panic!("unimplemented >32bit discr size: {}", discr_size);
                                    }

                                    // TODO: handle signed vs unsigned here as well, or just in the BinOps ?
                                    let discr_val = adt_def.variants[variant].disr_val.to_u64_unchecked() as i32;

                                    // set enum discr
                                    unsafe {
                                        debug!("emitting SetLocal({}) for CEnum Assign '{:?} = {:?}', discr: {:?}", dest.index.0, lvalue, rvalue, discr_val);
                                        let discr_val = BinaryenConst(self.module, BinaryenLiteralInt32(discr_val));
                                        let write_discr = BinaryenSetLocal(self.module, dest.index, discr_val);
                                        statements.push(write_discr);
                                    }
                                } else {
                                    panic!("tried to assign {:?} to Layout::CEnum", kind);
                                }
                            }

                            _ => panic!("unimplemented Assign Aggregate Adt {:?} on Layout {:?}", adt_def, dest_layout)
                        }
                    }

                    AggregateKind::Tuple => {
                        if operands.len() == 0 {
                            // TODO: in general, have a consistent strategy to handle the unit type assigns/returns
                            debug!("ignoring Assign '{:?} = {:?}'", lvalue, rvalue);
                        } else {
                            match *dest_layout {
                                Layout::Univariant { ref variant, .. } => {
                                    let dest_size = self.type_size(dest_ty) as i32 * 8;
                                    // NOTE: use the variant's min_size and alignment for dest_size ?
                                    debug!("allocating tuple in linear memory to SetLocal({}), size: {:?} bytes", dest.index.0, dest_size);
                                    let allocation = self.emit_alloca(dest.index, dest_size);
                                    statements.push(allocation);

                                    let offsets = ::std::iter::once(0).chain(variant.offset_after_field.iter().map(|s| s.bytes()));
                                    debug!("emitting Stores for tuple fields, values: {:?}", operands);
                                    self.emit_assign_fields(offsets, operands, statements);
                                }
                                _ => panic!("unimplemented Tuple Assign '{:?} = {:?}'", lvalue, rvalue)
                            }
                        }
                    }

                    _ => panic!("unimplemented Assign Aggregate {:?}", kind)
                }
            }

            Rvalue::Cast (ref kind, ref operand, _) => {
                if dest.offset.is_some() {
                    panic!("unimplemented '{:?}' Cast with offset", kind);
                }

                match *kind {
                    CastKind::Misc => {
                        let src = self.trans_operand(operand);
                        let src_ty = self.mir.operand_ty(*self.tcx, operand);
                        let src_layout = self.type_layout(src_ty);

                        // TODO: handle more of the casts (miri doesn't really handle every Misc cast either right now)
                        match (src_layout, &dest_ty.sty) {
                            (&Layout::Scalar { .. }, &ty::TyInt(_)) |
                            (&Layout::Scalar { .. }, &ty::TyUint(_)) => {
                                unsafe {
                                    debug!("emitting SetLocal({}) for Scalar Cast Assign '{:?} = {:?}'", dest.index.0, lvalue, rvalue);
                                    let copy_value = BinaryenSetLocal(self.module, dest.index, src);
                                    statements.push(copy_value);
                                }
                            }
                            (&Layout::CEnum { .. }, &ty::TyInt(_)) |
                            (&Layout::CEnum { .. }, &ty::TyUint(_)) => {
                                unsafe {
                                    debug!("emitting SetLocal({}) for CEnum Cast Assign '{:?} = {:?}'", dest.index.0, lvalue, rvalue);
                                    let copy_discr = BinaryenSetLocal(self.module, dest.index, src);
                                    statements.push(copy_discr);
                                }
                            }
                            _ => panic!("unimplemented '{:?}' Cast '{:?} = {:?}', for {:?} to {:?}", kind, lvalue, rvalue, src_layout, dest_ty.sty)
                        }
                    }
                    _ => panic!("unimplemented '{:?}' Cast '{:?} = {:?}'", kind, lvalue, rvalue)
                }
            }

            _ => panic!("unimplemented Assign '{:?} = {:?}'", lvalue, rvalue)
        }
    }

    // TODO: handle > 2GB allocations, when more types are handled and there's a consistent story around signed and unsigned
    fn emit_alloca(&mut self, dest: BinaryenIndex, dest_size: i32) -> BinaryenExpressionRef {
        self.frame.allocated = true;
        unsafe {
            let sp = self.emit_sp();
            let read_sp = BinaryenLoad(self.module, 4, 0, 0, 0, BinaryenInt32(), sp);

            let dest_size = BinaryenConst(self.module, BinaryenLiteralInt32(dest_size));
            let decr_sp = BinaryenBinary(self.module, BinaryenSubInt32(), read_sp, dest_size);
            let write_sp = BinaryenStore(self.module, 4, 0, 0, sp, decr_sp);
            let write_local = BinaryenSetLocal(self.module, dest, write_sp);
            write_local
        }
    }

    fn emit_sp(&self) -> BinaryenExpressionRef {
        unsafe {
            BinaryenConst(self.module, BinaryenLiteralInt32(STACK_POINTER_ADDRESS))
        }
    }

    fn emit_read_sp(&self) -> BinaryenExpressionRef {
        unsafe {
            BinaryenLoad(self.module, 4, 0, 0, 0, BinaryenInt32(), self.emit_sp())
        }
    }

    fn emit_assign_fields<I>(&mut self,
                             offsets: I,
                             operands: &[Operand<'tcx>],
                             statements: &mut Vec<BinaryenExpressionRef>)
            where I: IntoIterator<Item = u64> {
        unsafe {
            let read_sp = self.emit_read_sp();

            for (offset, operand) in offsets.into_iter().zip(operands) {
                // let operand_ty = self.mir.operand_ty(*self.tcx, operand);
                // TODO: match on the operand_ty to know how many bytes to store, not just i32s
                let src = self.trans_operand(operand);
                let write_field = BinaryenStore(self.module, 4, offset as u32, 0, read_sp, src);
                statements.push(write_field);
            }
        }
    }

    fn trans_lval(&mut self, lvalue: &Lvalue<'tcx>) -> BinaryenLvalue {
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
                let base = self.trans_lval(&projection.base);
                let base_ty = self.mir.lvalue_ty(*self.tcx, &projection.base).to_ty(*self.tcx);
                let base_layout = self.type_layout(base_ty);

                match projection.elem {
                    ProjectionElem::Deref => {
                        if base.offset.is_none() {
                            return BinaryenLvalue::new(base.index, None, LvalueExtra::None);
                        }
                        panic!("unimplemented Deref {:?}", lvalue);
                    }
                    ProjectionElem::Field(ref field, _) => {
                        let variant = match *base_layout {
                            Layout::Univariant { ref variant, .. } => variant,
                            Layout::General { ref variants, ..} => {
                                if let LvalueExtra::DowncastVariant(variant_idx) = base.extra {
                                    &variants[variant_idx]
                                } else {
                                    panic!("field access on enum had no variant index");
                                }
                            }
                            _ => panic!("unimplemented Field Projection: {:?}", projection)
                        };

                        let offset = variant.field_offset(field.index()).bytes() as u32;
                        return BinaryenLvalue::new(base.index, base.offset, LvalueExtra::None).offset(offset);
                    }
                    ProjectionElem::Downcast(_, variant) => {
                        match *base_layout {
                            Layout::General { discr, .. } => {
                                assert!(base.offset.is_none(), "unimplemented Downcast Projection with offset");

                                let offset = discr.size().bytes() as u32;
                                return BinaryenLvalue::new(base.index, Some(offset), LvalueExtra::DowncastVariant(variant));
                            }
                            _ => panic!("unimplemented Downcast Projection: {:?}", projection)
                        }
                    }
                    _ => panic!("unimplemented Projection: {:?}", projection)
                }
            }
            _ => panic!("unimplemented Lvalue: {:?}", lvalue)
        };

        BinaryenLvalue::new(BinaryenIndex(i), None, LvalueExtra::None)
    }

    fn trans_operand(&mut self, operand: &Operand<'tcx>) -> BinaryenExpressionRef {
        match *operand {
            Operand::Consume(ref lvalue) => {
                let binaryen_lvalue = self.trans_lval(lvalue);
                let lval_ty = self.mir.lvalue_ty(*self.tcx, lvalue);
                let t = lval_ty.to_ty(*self.tcx);
                let t = rust_ty_to_binaryen(t);

                unsafe {
                    match binaryen_lvalue.offset {
                        Some(offset) => {
                            debug!("emitting GetLocal({}) + Load for '{:?}'", binaryen_lvalue.index.0, lvalue);
                            let ptr = BinaryenGetLocal(self.module, binaryen_lvalue.index, t);
                            // TODO: match on the field ty to know how many bytes to read, not just i32s
                            BinaryenLoad(self.module, 4, 0, offset, 0, BinaryenInt32(), ptr)
                        }
                        None => {
                            // debug!("emitting GetLocal for '{:?}'", lvalue);
                            BinaryenGetLocal(self.module, binaryen_lvalue.index, t)
                        }
                    }
                }
            }
            Operand::Constant(ref c) => {
                match c.literal {
                    Literal::Value { ref value } => {
                        // TODO: handle more Rust types here
                        unsafe {
                            let lit = match *value {
                                ConstVal::Integral(ConstInt::Isize(ConstIsize::Is32(val))) |
                                ConstVal::Integral(ConstInt::I32(val)) => {
                                    BinaryenLiteralInt32(val)
                                }
                                // TODO: Since we're at the wasm32 stage, and until wasm64, it's probably best if isize is always i32 ?
                                ConstVal::Integral(ConstInt::Isize(ConstIsize::Is64(val))) => {
                                    BinaryenLiteralInt32(val as i32)
                                }
                                ConstVal::Integral(ConstInt::I64(val)) => {
                                    BinaryenLiteralInt64(val)
                                }
                                ConstVal::Bool(val) => {
                                    let val = if val { 1 } else { 0 };
                                    BinaryenLiteralInt32(val)
                                }
                                _ => panic!("unimplemented value: {:?}", value)
                            };
                            BinaryenConst(self.module, lit)
                        }

                    }
                    Literal::Promoted { .. } => {
                        panic!("unimplemented Promoted Literal: {:?}", c)
                    }
                    _ => panic!("unimplemented Constant Literal {:?}", c)
                }
            }
        }
    }

    #[inline]
    fn type_size(&self, ty: Ty<'tcx>) -> usize {
        let substs = self.tcx.mk_substs(Substs::empty());
        self.type_size_with_substs(ty, substs)
    }

    // Imported from miri
    #[inline]
    fn type_size_with_substs(&self, ty: Ty<'tcx>, substs: &'tcx Substs<'tcx>) -> usize {
        self.type_layout_with_substs(ty, substs).size(&self.tcx.data_layout).bytes() as usize
    }

    #[inline]
    fn type_layout(&self, ty: Ty<'tcx>) -> &'tcx Layout {
        let substs = self.tcx.mk_substs(Substs::empty());
        self.type_layout_with_substs(ty, substs)
    }

    // Imported from miri and slightly modified to adapt to our monomorphize api
    fn type_layout_with_substs(&self, ty: Ty<'tcx>, substs: &'tcx Substs<'tcx>) -> &'tcx Layout {
        // TODO(solson): Is this inefficient? Needs investigation.
        let ty = monomorphize::apply_ty_substs(self.tcx, substs, ty);

        self.tcx.normalizing_infer_ctxt(ProjectionMode::Any).enter(|infcx| {
            // TODO(solson): Report this error properly.
            ty.layout(&infcx).unwrap()
        })
    }

    fn trans_fn_call(&mut self,
                    func: &Operand<'tcx>,
                    args: &Vec<Operand<'tcx>>) -> (BinaryenExpressionRef, BinaryenCallKind) {
        let frame = Frame::new();
        let (func, ret_ty, call_kind) = self.trans_fn_name_direct(func, frame).expect(&format!("untranslated fn call to {:?}", func));
        let binaryen_args: Vec<_> = args.iter().map(|a| self.trans_operand(a)).collect();

        let call = unsafe {
            match call_kind {
                BinaryenCallKind::Direct => {
                    BinaryenCall(self.module,
                                 func,
                                 binaryen_args.as_ptr(),
                                 BinaryenIndex(binaryen_args.len() as _),
                                 ret_ty)
                }
                BinaryenCallKind::Import => {
                    BinaryenCallImport(self.module,
                                       func,
                                       binaryen_args.as_ptr(),
                                       BinaryenIndex(binaryen_args.len() as _),
                                       ret_ty)
                }
            }
        };

        (call, call_kind)
    }

    fn trans_fn_name_direct(&mut self, operand: &Operand<'tcx>, frame: Frame) -> Option<(*const c_char, BinaryenType, BinaryenCallKind)> {
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

                            match fn_name.as_ref() {
                                "wasm::::print_i32" | "wasm::::_print_i32" => {
                                    // extern wasm functions
                                    fn_sig = sig.clone();
                                    call_kind = BinaryenCallKind::Import;
                                    self.import_wasm_extern(fn_did, sig);
                                }
                                _ => {
                                    let is_trait_method = substs.self_ty().is_some();

                                    let (nid, substs, sig) = if !is_trait_method {
                                        let nid = self.tcx.map.as_local_node_id(fn_did).expect("");
                                        (nid, *substs, sig)
                                    } else {
                                        let (resolved_def_id, resolved_substs) = traits::resolve_trait_method(self.tcx, fn_did, substs);
                                        let nid = self.tcx.map.as_local_node_id(resolved_def_id).expect("");
                                        let ty = self.tcx.lookup_item_type(resolved_def_id).ty;
                                        // TODO: investigate rustc trans use of liberate_bound_regions or similar here
                                        let sig = ty.fn_sig().skip_binder();

                                        fn_did = resolved_def_id;
                                        (nid, resolved_substs, sig)
                                    };

                                    let mir = &self.mir_map.map[&nid];

                                    fn_sig = monomorphize::apply_param_substs(self.tcx, substs, sig);

                                    // mark the fn defid seen to not have translated twice
                                    // TODO: verify this more thoroughly, works for our limited tests right now
                                    if *sig != fn_sig {
                                        let fn_name = sanitize_symbol(&self.tcx.item_path_str(fn_did));
                                        let fn_name = CString::new(fn_name).expect("");
                                        self.fun_names.insert((fn_did, sig.clone()), fn_name);
                                    }

                                    // This simple check is also done in trans() but doing it here helps have a clearer debug log
                                    if !self.fun_names.contains_key(&(fn_did, fn_sig.clone())) {
                                        let mut ctxt = BinaryenFnCtxt {
                                            tcx: self.tcx,
                                            mir_map: self.mir_map,
                                            mir: mir,
                                            did: fn_did,
                                            sig: &fn_sig,
                                            module: self.module,
                                            entry_fn: self.entry_fn,
                                            fun_types: &mut self.fun_types,
                                            fun_names: &mut self.fun_names,
                                            c_strings: &mut self.c_strings,
                                            frame: frame,
                                        };

                                        debug!("translating monomorphized fn {:?}", self.tcx.item_path_str(fn_did));
                                        ctxt.trans();
                                        debug!("done translating monomorphized {:?}, continuing translation of fn {:?}",
                                            self.tcx.item_path_str(fn_did), self.tcx.item_path_str(self.did));
                                    }
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
                            panic!("unimplemented ty {:?} for {:?}", ty, def_id);
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
            //        track its initial size, etc and extract that into its own abstraction
            //     -> temporarily, just ask for one 64K page
            BinaryenSetMemory(self.module, BinaryenIndex(1), BinaryenIndex(1),
                              ptr::null(), ptr::null(), ptr::null(), ptr::null(),
                              BinaryenIndex(0));

            let stack_top = BinaryenConst(self.module, BinaryenLiteralInt32(0xFFFF));
            let stack_init = BinaryenStore(self.module, 4, 0, 0, self.emit_sp(), stack_top);
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
            '<' | '>' | ' ' | '(' | ')' => '_',
            _ => c
        }
    }).collect()
}

#[derive(Debug)]
enum BinaryenCallKind {
    Direct,
    Import,
    // Indirect // unimplemented at the moment
}

enum BinaryenBlockKind {
    Default,
    Switch(BinaryenExpressionRef)
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct BinaryenLvalue {
    index: BinaryenIndex,
    offset: Option<u32>,
    extra: LvalueExtra,
}

impl BinaryenLvalue {
    fn new(index: BinaryenIndex, offset: Option<u32>, extra: LvalueExtra) -> Self {
        BinaryenLvalue {
            index: index,
            offset: offset,
            extra: extra
        }
    }

    fn offset(&self, offset: u32) -> Self {
        let offset = match self.offset {
            None => Some(offset),
            Some(base_offset) => Some(base_offset + offset)
        };

        Self::new(self.index, offset, self.extra)
    }
}

// The following is imported from miri as well
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum LvalueExtra {
    None,
    // Length(u64),
    // TODO(solson): Vtable(memory::AllocId),
    DowncastVariant(usize),
}

trait IntegerExt {
    fn size(self) -> Size;
}

impl IntegerExt for layout::Integer {
    fn size(self) -> Size {
        use rustc::ty::layout::Integer::*;
        match self {
            I1 | I8 => Size::from_bits(8),
            I16 => Size::from_bits(16),
            I32 => Size::from_bits(32),
            I64 => Size::from_bits(64),
        }
    }
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
