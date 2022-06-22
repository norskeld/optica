use std::sync::atomic::{AtomicUsize, Ordering};

use crate::ast::typed::*;

/// Global atomic function ID.
static FUNC_ID: AtomicUsize = AtomicUsize::new(0);

/// Retrieves and increments the next free function ID.
pub fn function_id() -> FunctionId {
  FUNC_ID.fetch_add(1, Ordering::SeqCst)
}
