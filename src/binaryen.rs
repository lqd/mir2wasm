use libc::{c_void, c_char, size_t};

pub type BinaryenIndex = u32;
pub type BinaryenType = u32;
pub type BinaryenOp = u32;

pub type BinaryenModuleRef = *mut c_void;
pub type BinaryenFunctionTypeRef = *mut c_void;
pub type BinaryenExpressionRef = *mut c_void;
pub type BinaryenFunctionRef = *mut c_void;
pub type BinaryenImportRef = *mut c_void;
pub type BinaryenExportRef = *mut c_void;

pub struct BinaryenLiteral {
    type_: i32,
    contents: i64,
}

pub type RelooperRef  = *mut c_void;
pub type RelooperBlockRef = *mut c_void;

extern {
    // Basic types
    pub fn BinaryenNone() -> BinaryenType;
    pub fn BinaryenInt32() -> BinaryenType;
    pub fn BinaryenInt64() -> BinaryenType;
    pub fn BinaryenFloat32() -> BinaryenType;
    pub fn BinaryenFloat64() -> BinaryenType;

    // Literals

    pub fn BinaryenLiteralInt32(x: i32) -> BinaryenLiteral;
    pub fn BinaryenLiteralInt64(x: i64) -> BinaryenLiteral;
    pub fn BinaryenLiteralFloat32(x: f32) -> BinaryenLiteral;
    pub fn BinaryenLiteralFloat64(x: f64) -> BinaryenLiteral;
    pub fn BinaryenLiteralFloat32Bits(x: i32) -> BinaryenLiteral;
    pub fn BinaryenLiteralFloat64Bits(x: i64) -> BinaryenLiteral;

    // Modules
    pub fn BinaryenModuleCreate() -> BinaryenModuleRef;
    pub fn BinaryenModuleDispose(module: BinaryenModuleRef);

    // Function types

    pub fn BinaryenAddFunctionType(module: BinaryenModuleRef, name: *const c_char, result: BinaryenType, paramTypes: *const BinaryenType, numParams: BinaryenIndex) -> BinaryenFunctionTypeRef;

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

    // Expressions

    pub fn BinaryenBlock(module: BinaryenModuleRef, name: *const c_char, children: *const BinaryenExpressionRef, numChildren: BinaryenIndex) -> BinaryenExpressionRef;
    pub fn BinaryenIf(module: BinaryenModuleRef, condition: BinaryenExpressionRef, ifTrue: BinaryenExpressionRef, ifFalse: BinaryenExpressionRef) -> BinaryenExpressionRef;
    pub fn BinaryenLoop(module: BinaryenModuleRef, out: *const c_char, in_: *const c_char, body: BinaryenExpressionRef) -> BinaryenExpressionRef;
    pub fn BinaryenBreak(module: BinaryenModuleRef, name: *const c_char, condition: BinaryenExpressionRef, value: BinaryenExpressionRef) -> BinaryenExpressionRef;
    pub fn BinaryenSwitch(module: BinaryenModuleRef, names: *const *const c_char, numNames: BinaryenIndex, defaultName: *const c_char, condition: BinaryenExpressionRef, value: BinaryenExpressionRef) -> BinaryenExpressionRef;
    pub fn BinaryenCall(module: BinaryenModuleRef, target: *const c_char, operands: *const BinaryenExpressionRef, numOperands: BinaryenIndex, returnType: BinaryenType) -> BinaryenExpressionRef;
    pub fn BinaryenCallImport(module: BinaryenModuleRef, target: *const c_char, operands: *const BinaryenExpressionRef, numOperands: BinaryenIndex, returnType: BinaryenType) -> BinaryenExpressionRef;
    pub fn BinaryenCallIndirect(module: BinaryenModuleRef, target: BinaryenExpressionRef, operands: *const BinaryenExpressionRef, numOperands: BinaryenIndex, type_: BinaryenFunctionTypeRef) -> BinaryenExpressionRef;
    pub fn BinaryenGetLocal(module: BinaryenModuleRef, index: BinaryenIndex, type_: BinaryenType) -> BinaryenExpressionRef;
    pub fn BinaryenSetLocal(module: BinaryenModuleRef, index: BinaryenIndex, value: BinaryenExpressionRef) -> BinaryenExpressionRef;
    pub fn BinaryenLoad(module: BinaryenModuleRef, bytes: u32, signed_: u8, offset: u32, align: u32, type_: BinaryenType, ptr: BinaryenExpressionRef) -> BinaryenExpressionRef;
    pub fn BinaryenStore(module: BinaryenModuleRef, bytes: u32, offset: u32, align: u32, ptr: BinaryenExpressionRef, value: BinaryenExpressionRef) -> BinaryenExpressionRef;
    pub fn BinaryenConst(module: BinaryenModuleRef, value: BinaryenLiteral) -> BinaryenExpressionRef;
    pub fn BinaryenUnary(module: BinaryenModuleRef, op: BinaryenOp, value: BinaryenExpressionRef) -> BinaryenExpressionRef;
    pub fn BinaryenBinary(module: BinaryenModuleRef, op: BinaryenOp, left: BinaryenExpressionRef, right: BinaryenExpressionRef) -> BinaryenExpressionRef;
    pub fn BinaryenSelect(module: BinaryenModuleRef, condition: BinaryenExpressionRef, ifTrue: BinaryenExpressionRef, ifFalse: BinaryenExpressionRef) -> BinaryenExpressionRef;
    pub fn BinaryenReturn(module: BinaryenModuleRef, value: BinaryenExpressionRef) -> BinaryenExpressionRef;
    pub fn BinaryenHost(module: BinaryenModuleRef, op: BinaryenOp, name: *const c_char, operands: *const BinaryenExpressionRef, numOperands: BinaryenIndex) -> BinaryenExpressionRef;
    pub fn BinaryenNop(module: BinaryenModuleRef) -> BinaryenExpressionRef;
    pub fn BinaryenUnreachable(module: BinaryenModuleRef) -> BinaryenExpressionRef;

    // Functions

    pub fn BinaryenAddFunction(module: BinaryenModuleRef, name: *const c_char, type_: BinaryenFunctionTypeRef, localTypes: *const BinaryenType, numLocalTypes: BinaryenIndex, body: BinaryenExpressionRef) -> BinaryenFunctionRef;

    // Imports

    pub fn BinaryenAddImport(module: BinaryenModuleRef, internalName: *const c_char, externalModuleName: *const c_char, externalBaseName: *const char, type_: BinaryenFunctionTypeRef) -> BinaryenImportRef;

    // Exports

    pub fn BinaryenAddExport(module: BinaryenModuleRef, internalName: *const c_char, externalName: *const c_char) -> BinaryenExportRef;

    // Function table. One per module

    pub fn BinaryenSetFunctionTable(module: BinaryenModuleRef, functions: *const BinaryenFunctionRef, numFunctions: BinaryenIndex);

    // Memory

    pub fn BinaryenSetMemory(module: BinaryenModuleRef, initial: BinaryenIndex, maximum: BinaryenIndex, exportName: *const c_char, segments: *const *const c_char, segmentOffsets: *const BinaryenIndex, segmentSizes: *const BinaryenIndex, numSegments: BinaryenIndex);

    // Start function

    pub fn BinaryenSetStart(module: BinaryenModuleRef, start: BinaryenFunctionRef);

    // Module Operations

    pub fn BinaryenModulePrint(module: BinaryenModuleRef);

    pub fn BinaryenModuleValidate(module: BinaryenModuleRef) -> i32;

    pub fn BinaryenModuleOptimize(module: BinaryenModuleRef);

    pub fn BinaryenModuleWrite(module: BinaryenModuleRef, output: *const c_char, outputSize: size_t) -> size_t;

    pub fn BinaryenModuleRead(input: *const c_char, inputSize: size_t) -> BinaryenModuleRef;

    // CFG / Relooper

    pub fn RelooperCreate() -> RelooperRef;

    pub fn RelooperAddBlock(relooper: RelooperRef, code: BinaryenExpressionRef) -> RelooperBlockRef;

    pub fn RelooperAddBranch(from: RelooperBlockRef, to: RelooperBlockRef, condition: BinaryenExpressionRef, code: BinaryenExpressionRef);

    pub fn RelooperRenderAndDispose(relooper: RelooperRef, entry: RelooperBlockRef, labelHelper: BinaryenIndex, module: BinaryenModuleRef) -> BinaryenExpressionRef;

}
