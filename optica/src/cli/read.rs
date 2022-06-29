use crate::ast::typed::Value;
use crate::errors::LangError;
use crate::runtime::Runtime;
use crate::utils::path;

pub fn read(path: &str) -> Result<Value, LangError> {
  let mut runtime = Runtime::new();

  let (file_path, module_name) = path::normalize_path(path)?;

  runtime.init()?;
  runtime.include_file(&file_path)?;
  runtime.import_module(&module_name)?;

  runtime.eval_expression(&format!("{module_name}.main"))
}
