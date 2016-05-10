#[link(name = "binaryen")]
extern {
    type BinaryenIndex = u32;
    type BinaryenType = u32;

    fn BinaryenNone() -> BinaryenType;
}
