macro_rules! span {
  ($pair:expr) => {
    (($pair.0 as usize)..($pair.1 as usize))
  };
}
