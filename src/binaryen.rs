pub type BinaryenIndex = u32;
pub type BinaryenType = u32;

#[link(name = "binaryen")]
extern {
    pub fn BinaryenNone() -> BinaryenType;
}
