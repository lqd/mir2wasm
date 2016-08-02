#![stable(feature = "rust1", since = "1.0.0")]

#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord/*, Debug, Hash*/)]
#[stable(feature = "rust1", since = "1.0.0")]
pub enum Option<T> {
    /// No value
    #[stable(feature = "rust1", since = "1.0.0")]
    None,
    /// Some value `T`
    #[stable(feature = "rust1", since = "1.0.0")]
    Some(#[stable(feature = "rust1", since = "1.0.0")] T)
}
