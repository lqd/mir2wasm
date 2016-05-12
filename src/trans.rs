use error::*;
use rustc_mir;
use rustc::mir::mir_map::MirMap;
use rustc::ty::{self, TyCtxt};
use rustc::hir::intravisit::{self, Visitor, FnKind};
use rustc::hir::{FnDecl, Block};
use syntax::ast::NodeId;
use syntax::codemap::Span;

pub fn translate_crate<'tcx>(tcx: &TyCtxt<'tcx>,
                             mir_map: &MirMap<'tcx>) -> Result<()> {
    let _ignore = tcx.dep_graph.in_ignore();

    let ref mut v = HirVisitor {
        tcx: tcx,
        mir_map: mir_map
    };

    tcx.map.krate().visit_all_items(v);

    Ok(())
}

struct HirVisitor<'v, 'tcx: 'v> {
    tcx: &'v TyCtxt<'tcx>,
    mir_map: &'v MirMap<'tcx>
}

impl<'v, 'tcx> Visitor<'v> for HirVisitor<'v, 'tcx> {
    fn visit_fn(&mut self, fk: FnKind<'v>, fd: &'v FnDecl,
                b: &'v Block, s: Span, id: NodeId) {
        debug!("visiting fn {:?}", fd);

        let mir = self.mir_map.map.get(&id);
        //let did = self.cx.map.local_def_id(&id);
        //let ty = self.cx.lookup_item_type(did);
        // look up def_id lookup_item_type
        // get type with fn_sig/fn_args/fn_ret


        //debug!("fn mir: {:?}", mir);

        intravisit::walk_fn(self, fk, fd, b, s)
    }
}
