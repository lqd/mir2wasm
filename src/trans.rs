use error::*;
use rustc_mir;
use rustc::mir::mir_map::MirMap;
use rustc::ty::{self, TyCtxt};
use rustc::hir::intravisit::{self, Visitor, FnKind};
use rustc::hir::{FnDecl, Block};
use syntax::ast::NodeId;
use syntax::codemap::Span;
use std::ffi::CString;
use std::ptr;
use binaryen::*;

pub fn translate_crate<'tcx>(tcx: &TyCtxt<'tcx>,
                             mir_map: &MirMap<'tcx>) -> Result<()> {

    for (&id, mir) in &mir_map.map {
        for attr in tcx.map.attrs(id) {
            let item = tcx.map.expect_item(id);
            println!("Processing function: {}", item.name);

            unsafe {
                let module = BinaryenModuleCreate();

                let param = BinaryenInt32();
                let ii = BinaryenAddFunctionType(module, CString::new("iii").unwrap().as_ptr(), BinaryenInt32(), &param, 1);

                let add = BinaryenBinary(module, BinaryenAdd(), BinaryenGetLocal(module, 0, BinaryenInt32()), BinaryenGetLocal(module, 0, BinaryenInt32()));

                let adder = BinaryenAddFunction(module, CString::new(item.name.as_str().as_bytes()).unwrap().as_ptr(), ii, ptr::null(), 0, add);

                BinaryenModulePrint(module);
            }

        }
    }

    Ok(())
}

