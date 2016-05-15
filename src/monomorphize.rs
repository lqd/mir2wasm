use rustc::ty::subst::{Subst, Substs};
use rustc::ty::{TyCtxt, TypeFoldable};
use rustc::infer::normalize_associated_type;

pub fn apply_param_substs<'tcx,T>(tcx: &TyCtxt<'tcx>,
                                  param_substs: &Substs<'tcx>,
                                  value: &T)
                                  -> T
    where T : TypeFoldable<'tcx>
{
    let substituted = value.subst(tcx, param_substs);
    normalize_associated_type(tcx, &substituted)
}
