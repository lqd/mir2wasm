#![stable(feature = "rust1", since = "1.0.0")]

/// `Result` is a type that represents either success (`Ok`) or failure (`Err`).
///
/// See the [`std::result`](index.html) module documentation for details.
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord/*, Debug, Hash*/)]
#[must_use]
#[stable(feature = "rust1", since = "1.0.0")]
pub enum Result<T, E> {
    /// Contains the success value
    #[stable(feature = "rust1", since = "1.0.0")]
    Ok(#[stable(feature = "rust1", since = "1.0.0")] T),

    /// Contains the error value
    #[stable(feature = "rust1", since = "1.0.0")]
    Err(#[stable(feature = "rust1", since = "1.0.0")] E),
}
