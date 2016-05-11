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

    let ref mut v = HirVisitor;

    tcx.map.krate().visit_all_items(v);

    Ok(())
}

struct HirVisitor;

impl<'v> Visitor<'v> for HirVisitor {
    fn visit_fn(&mut self, fk: FnKind<'v>, fd: &'v FnDecl,
                b: &'v Block, s: Span, _: NodeId) {
        debug!("{:?}", fd);
        intravisit::walk_fn(self, fk, fd, b, s)
    }
}
