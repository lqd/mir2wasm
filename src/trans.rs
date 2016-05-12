use error::*;
use rustc_mir;
use rustc::mir::mir_map::MirMap;
use rustc::ty::{self, TyCtxt};
use rustc::hir;
use rustc::hir::intravisit::{self, Visitor, FnKind};
use rustc::hir::{FnDecl, Block};
use syntax::ast::NodeId;
use syntax::codemap::Span;
use std::ffi::CString;
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
    fun_types: HashMap<&'static str, BinaryenFunctionTypeRef>,
}

impl<'v, 'tcx> Visitor<'v> for HirVisitor<'v, 'tcx> {
    fn visit_fn(&mut self, fk: FnKind<'v>, fd: &'v FnDecl,
                b: &'v Block, s: Span, id: NodeId) {

        let item = self.tcx.map.expect_item(id);

        println!("Processing function ({}): {}", id, item.name);

        unsafe {
            let mut return_type : BinaryenType;

            let sig = match fd.output {
                hir::FunctionRetTy::Return(ref x) => {
                    return_type = BinaryenInt32();
                    "i"
                },
                _ => {
                    return_type = BinaryenNone();
                    "v"
                }
            };

            if !self.fun_types.contains_key(sig) {
                let param = BinaryenInt32();
                let ty = BinaryenAddFunctionType(self.module, CString::new(sig).unwrap().as_ptr(), return_type, &param, 1);
                self.fun_types.insert(sig, ty);
            }

            let add = BinaryenBinary(self.module, BinaryenAdd(), BinaryenGetLocal(self.module, 0, BinaryenInt32()), BinaryenGetLocal(self.module, 0, BinaryenInt32()));

            BinaryenAddFunction(self.module, CString::new(item.name.as_str().as_bytes()).unwrap().as_ptr(), *self.fun_types.get(sig).unwrap(), ptr::null(), 0, add);
        }

        intravisit::walk_fn(self, fk, fd, b, s)
    }
}
