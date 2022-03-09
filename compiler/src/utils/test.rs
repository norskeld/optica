#[macro_export]
macro_rules! assert_ok {
  ($result: expr, $token: expr) => {
    match &$result {
      | Ok((_, item)) => {
        assert_eq!(*item, $token);
      },
      | Err(_) => {
        panic!("{:?}", $result);
      },
    }
  };
}
