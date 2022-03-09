/// Default type for integer values.
pub type Int = isize;

/// Default type for floating point values on targets with pointer width of 64 bits.
#[cfg(target_pointer_width = "64")]
pub type Float = f64;

/// Default type for floating point values on targets with pointer width of 32 bits.
#[cfg(target_pointer_width = "32")]
pub type Float = f32;
