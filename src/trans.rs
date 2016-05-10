use error::*;
use rustc_mir;
use rustc::mir::mir_map::MirMap;
use rustc::ty::{self, TyCtxt};

pub fn translate_crate<'tcx>(tcx: &TyCtxt<'tcx>,
                             mir_map: &MirMap<'tcx>) -> Result<()> {
    for (&id, mir) in &mir_map.map {
        debug!("visiting node: {:?}", id);
    }

    Ok(())
}
