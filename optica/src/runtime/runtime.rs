use std::collections::HashMap;

use super::Interpreter;
use crate::ast::typed::*;
use crate::errors::*;
use crate::lexer::Lexer;
use crate::loader::*;
use crate::parser::Parser;
use crate::source::{SourceCode, SourceFile};
use crate::typechecker::Typechecker;

#[derive(Debug)]
pub struct Runtime {
  pub interpreter: Interpreter,
  pub typechecker: Typechecker,
  pub loaded_modules: HashMap<String, LoadedModule>,
  pub runtime_modules: HashMap<String, RuntimeModule>,
  pub typed_modules: HashMap<String, TypedModule>,
}

impl Runtime {
  pub fn new() -> Runtime {
    Runtime {
      interpreter: Interpreter::new(),
      typechecker: Typechecker::new(SourceCode::from_str("")),
      loaded_modules: HashMap::new(),
      typed_modules: HashMap::new(),
      runtime_modules: HashMap::new(),
    }
  }

  /// Evaluates an expression like, e.g. `1 + 2`.
  pub fn eval_expression(&mut self, expr: &str) -> Result<Value, LangError> {
    let code = SourceCode::from_str(expr);
    let lexer = Lexer::new(&code);
    let mut parser = Parser::new(lexer);
    let expression = parser.parse_expression()?;
    let typed_expression = self
      .typechecker
      .with(code)
      .typecheck_expression(&expression)?;

    let value = self.interpreter.eval_expression(&typed_expression)?;

    Ok(value)
  }

  /// Evaluates a statement, e.g.:
  ///
  /// - `x = 1`
  /// - `sum a b = a + b`
  /// - `type Boolean = Bool`
  /// - `data List a = Cons a (List a) | Nil`
  pub fn eval_statement(&mut self, stmt: &str) -> Result<Option<Value>, LangError> {
    let code = SourceCode::from_str(stmt);
    let lexer = Lexer::new(&code);
    let mut parser = Parser::new(lexer);
    let statement = parser.parse_statement()?;
    let typed_statements = self
      .typechecker
      .with(code)
      .typecheck_statement(&statement)?;

    let mut value = None;

    for typed_statement in &typed_statements {
      value = self.interpreter.eval_statement(typed_statement)?;

      if let Some(ty) = typed_statement.get_type() {
        self
          .typechecker
          .add_port(typed_statement.get_name(), ty.clone());
      }
    }

    Ok(value)
  }

  /// Evaluates a module.
  pub fn eval_module(&mut self, module: &str, name: &str) -> Result<(), LangError> {
    let code = SourceCode::from_str(module);
    let lexer = Lexer::new(&code);
    let mut parser = Parser::new(lexer);
    let module = parser.parse_module()?;

    ModuleLoader::include_module(
      self,
      SourceFile {
        name: name.to_string(),
        path: name.to_string(),
        source: code,
      },
      module,
    )?;

    self.import_module(name)
  }

  /// Loads all source files in the folder and checks for missing dependencies.
  ///
  /// Note: it doesn't import modules, you have to import them with `Self::import_module`.
  pub fn include_files(&mut self, folder_path: &str) -> Result<(), LangError> {
    ModuleLoader::include_folder(self, folder_path)
  }

  /// Loads a single source file and checks its dependencies.
  ///
  /// Note: it doesn't import the module, you have to import it with `Self::import_module`.
  pub fn include_file(&mut self, file_path: &str) -> Result<(), LangError> {
    ModuleLoader::include_file(self, "", file_path)
  }

  /// Import a module, previously loaded with `Self::include_file` or `Self::include_files`, into
  /// the current environment.
  pub fn import_module(&mut self, module_name: &str) -> Result<(), LangError> {
    self.import_module_as(module_name, module_name)
  }

  /// Import a module, previously loaded with `Self::include_file` or `Self::include_files`, into
  /// the current environment with an alias.
  pub fn import_module_as(&mut self, module_name: &str, alias: &str) -> Result<(), LangError> {
    if !self.runtime_modules.contains_key(module_name) {
      self.load_typed_module(module_name)?;
    }

    self.load_runtime_module(module_name)?;
    self.import_module_definitions(module_name, alias)?;

    Ok(())
  }

  fn import_module_definitions(&mut self, module_name: &str, alias: &str) -> Result<(), LangError> {
    self.runtime_modules.get(module_name).map_or_else(
      // Expected module to be already loaded.
      || {
        Err(
          LoaderError::NotLoadedModule {
            module: module_name.to_string(),
          }
          .wrap(),
        )
      },
      |module| {
        for (definition, value) in &module.definitions {
          let name = if alias.is_empty() {
            definition.clone()
          } else {
            format!("{alias}.{definition}")
          };

          self.typechecker.add_port(&name, value.get_type());
          self.interpreter.stack.add(&name, value.clone());
        }

        Ok(())
      },
    )
  }

  fn load_typed_module(&mut self, module_name: &str) -> Result<(), LangError> {
    let dependencies = self
      .loaded_modules
      .get(module_name)
      .ok_or_else(|| {
        LoaderError::MissingModule {
          module: module_name.to_string(),
        }
        .wrap()
      })?
      .dependencies
      .clone();

    // Load dependencies.
    for dependency in &dependencies {
      if !self.typed_modules.contains_key(dependency) {
        self.load_typed_module(dependency)?;
      }
    }

    let module = self.loaded_modules.get(module_name).ok_or_else(|| {
      LoaderError::MissingModule {
        module: module_name.to_string(),
      }
      .wrap()
    })?;

    // Typecheck module.
    let mut typechecker = Typechecker::new(module.src.source.clone());
    let typed_module = typechecker.typecheck_module(&self.typed_modules, module)?;

    self
      .typed_modules
      .insert(module_name.to_string(), typed_module);

    Ok(())
  }

  fn load_runtime_module(&mut self, module_name: &str) -> Result<(), LangError> {
    let dependencies = self
      .typed_modules
      .get(module_name)
      .ok_or_else(|| {
        LoaderError::MissingModule {
          module: module_name.to_string(),
        }
        .wrap()
      })?
      .dependencies
      .clone();

    // Load dependencies.
    for dependency in &dependencies {
      if !self.runtime_modules.contains_key(dependency) {
        self.load_runtime_module(dependency)?;
      }
    }

    let mut interpreter = Interpreter::new();

    let runtime_module = self
      .typed_modules
      .get(module_name)
      .ok_or_else(|| {
        LoaderError::MissingModule {
          module: module_name.to_string(),
        }
        .wrap()
      })
      .and_then(|module| interpreter.eval_module(&self.runtime_modules, module))
      .and_then(|module| interpreter.eval_constants(module))?;

    self
      .runtime_modules
      .insert(module_name.to_string(), runtime_module);

    Ok(())
  }
}

impl Default for Runtime {
  fn default() -> Self {
    Self::new()
  }
}
