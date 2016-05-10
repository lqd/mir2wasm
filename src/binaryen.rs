use libc::{c_void, c_char};

pub type BinaryenIndex = u32;
pub type BinaryenType = u32;
pub type BinaryenOp = u32;

pub type BinaryenModuleRef = *mut c_void;
pub type BinaryenFunctionTypeRef = *mut c_void;

extern {
    // Basic types
    pub fn BinaryenNone() -> BinaryenType;
    pub fn BinaryenInt32() -> BinaryenType;
    pub fn BinaryenInt64() -> BinaryenType;
    pub fn BinaryenFloat32() -> BinaryenType;
    pub fn BinaryenFloat64() -> BinaryenType;

    // Modules
    pub fn BinaryenModuleCreate() -> BinaryenModuleRef;
    pub fn BinaryenModuleDispose(module: BinaryenModuleRef);

    // Function types

    pub fn BinaryenAddFunctionType(module: BinaryenModuleRef, name: *const c_char, result: BinaryenType, paramTypes: *mut BinaryenType, numParams: BinaryenIndex) -> BinaryenFunctionTypeRef;

    // Ops

    pub fn BinaryenClz() -> BinaryenOp;
    pub fn BinaryenCtz() -> BinaryenOp;
    pub fn BinaryenPopcnt() -> BinaryenOp;
    pub fn BinaryenNeg() -> BinaryenOp;
    pub fn BinaryenAbs() -> BinaryenOp;
    pub fn BinaryenCeil() -> BinaryenOp;
    pub fn BinaryenFloor() -> BinaryenOp;
    pub fn BinaryenTrunc() -> BinaryenOp;
    pub fn BinaryenNearest() -> BinaryenOp;
    pub fn BinaryenSqrt() -> BinaryenOp;
    pub fn BinaryenEqZ() -> BinaryenOp;
    pub fn BinaryenExtendSInt32() -> BinaryenOp;
    pub fn BinaryenExtentUInt32() -> BinaryenOp;
    pub fn BinaryenWrapInt64() -> BinaryenOp;
    pub fn BinaryenTruncSFloat32ToInt32() -> BinaryenOp;
    pub fn BinaryenTruncSFloat32ToInt64() -> BinaryenOp;
    pub fn BinaryenTruncUFloat32ToInt32() -> BinaryenOp;
    pub fn BinaryenTruncUFloat32ToInt64() -> BinaryenOp;
    pub fn BinaryenTruncSFloat64ToInt32() -> BinaryenOp;
    pub fn BinaryenTruncSFloat64ToInt64() -> BinaryenOp;
    pub fn BinaryenTruncUFloat64ToInt32() -> BinaryenOp;
    pub fn BinaryenTruncUFloat64ToInt64() -> BinaryenOp;
    pub fn BinaryenReinterpretFloat32() -> BinaryenOp;
    pub fn BinaryenReinterpretFloat64() -> BinaryenOp;
    pub fn BinaryenConvertSInt32ToFloat32() -> BinaryenOp;
    pub fn BinaryenConvertSInt32ToFloat64() -> BinaryenOp;
    pub fn BinaryenConvertUInt32ToFloat32() -> BinaryenOp;
    pub fn BinaryenConvertUInt32ToFloat64() -> BinaryenOp;
    pub fn BinaryenConvertSInt64ToFloat32() -> BinaryenOp;
    pub fn BinaryenConvertSInt64ToFloat64() -> BinaryenOp;
    pub fn BinaryenConvertUInt64ToFloat32() -> BinaryenOp;
    pub fn BinaryenConvertUInt64ToFloat64() -> BinaryenOp;
    pub fn BinaryenPromoteFloat32() -> BinaryenOp;
    pub fn BinaryenDemoteFloat64() -> BinaryenOp;
    pub fn BinaryenReinterpretInt32() -> BinaryenOp;
    pub fn BinaryenReinterpretInt64() -> BinaryenOp;
    pub fn BinaryenAdd() -> BinaryenOp;
    pub fn BinaryenSub() -> BinaryenOp;
    pub fn BinaryenMul() -> BinaryenOp;
    pub fn BinaryenDivS() -> BinaryenOp;
    pub fn BinaryenDivU() -> BinaryenOp;
    pub fn BinaryenRemS() -> BinaryenOp;
    pub fn BinaryenRemU() -> BinaryenOp;
    pub fn BinaryenAnd() -> BinaryenOp;
    pub fn BinaryenOr() -> BinaryenOp;
    pub fn BinaryenXor() -> BinaryenOp;
    pub fn BinaryenShl() -> BinaryenOp;
    pub fn BinaryenShrU() -> BinaryenOp;
    pub fn BinaryenShrS() -> BinaryenOp;
    pub fn BinaryenRotL() -> BinaryenOp;
    pub fn BinaryenRotR() -> BinaryenOp;
    pub fn BinaryenDiv() -> BinaryenOp;
    pub fn BinaryenCopySign() -> BinaryenOp;
    pub fn BinaryenMin() -> BinaryenOp;
    pub fn BinaryenMax() -> BinaryenOp;
    pub fn BinaryenEq() -> BinaryenOp;
    pub fn BinaryenNe() -> BinaryenOp;
    pub fn BinaryenLtS() -> BinaryenOp;
    pub fn BinaryenLtU() -> BinaryenOp;
    pub fn BinaryenLeS() -> BinaryenOp;
    pub fn BinaryenLeU() -> BinaryenOp;
    pub fn BinaryenGtS() -> BinaryenOp;
    pub fn BinaryenGtU() -> BinaryenOp;
    pub fn BinaryenGeS() -> BinaryenOp;
    pub fn BinaryenGeU() -> BinaryenOp;
    pub fn BinaryenLt() -> BinaryenOp;
    pub fn BinaryenLe() -> BinaryenOp;
    pub fn BinaryenGt() -> BinaryenOp;
    pub fn BinaryenGe() -> BinaryenOp;
    pub fn BinaryenPageSize() -> BinaryenOp;
    pub fn BinaryenCurrentMemory() -> BinaryenOp;
    pub fn BinaryenGrowMemory() -> BinaryenOp;
    pub fn BinaryenHasFeature() -> BinaryenOp;
}
