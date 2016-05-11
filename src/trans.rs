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
    let _ignore = tcx.dep_graph.in_ignore();

    let ref mut v = HirVisitor { mir_map: mir_map };

    tcx.map.krate().visit_all_items(v);

    Ok(())
}

struct HirVisitor<'a, 'tcx: 'a> {
    mir_map: &'a MirMap<'tcx>
}

impl<'v, 'a: 'tcx, 'tcx> Visitor<'v> for HirVisitor<'a, 'tcx> {
    fn visit_fn(&mut self, fk: FnKind<'v>, fd: &'v FnDecl,
                b: &'v Block, s: Span, id: NodeId) {
        debug!("visiting fn {:?}", fd);

        //vvvvv
        unsafe {
            let module = BinaryenModuleCreate();

            let param = BinaryenInt32();
            let ii = BinaryenAddFunctionType(module, CString::new("iii").unwrap().as_ptr(), BinaryenInt32(), &param, 1);

            let add = BinaryenBinary(module, BinaryenAdd(), BinaryenGetLocal(module, 0, BinaryenInt32()), BinaryenGetLocal(module, 0, BinaryenInt32()));

            let adder = BinaryenAddFunction(module, CString::new("adder").unwrap().as_ptr(), ii, ptr::null(), 0, add);

            BinaryenModulePrint(module);
        }
        //^^^^^

        let mir = self.mir_map.map.get(&id);

        //debug!("fn mir: {:?}", mir);

        intravisit::walk_fn(self, fk, fd, b, s)
    }
}
