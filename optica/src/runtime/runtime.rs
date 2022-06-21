use std::collections::HashMap;

use crate::ast;
use crate::ast::typed::Value;
use crate::errors::{LangError, LoaderError, Wrappable};
use crate::lexer::Lexer;
use crate::loader::{AnalyzedModule, LoadedModule, ModuleLoader, RuntimeModule};
use crate::parser::Parser;
use crate::source::{SourceCode, SourceFile};
use crate::typechecker::Typechecker;
use super::Interpreter;

#[derive(Debug)]
pub struct Runtime {
  pub interpreter: Interpreter,
  pub typechecker: Typechecker,
  pub loaded_modules: HashMap<String, LoadedModule>,
  pub analyzed_modules: HashMap<String, AnalyzedModule>,
  pub runtime_modules: HashMap<String, RuntimeModule>,
}

impl Runtime {
  pub fn new() -> Runtime {
    Self::fresh()
  }

  pub fn fresh() -> Runtime {
    Runtime {
      interpreter: Interpreter::new(),
      typechecker: Typechecker::new(SourceCode::from_str("")),
      loaded_modules: HashMap::new(),
      analyzed_modules: HashMap::new(),
      runtime_modules: HashMap::new(),
    }
  }

  /// Evaluates an expression like, e.g. `1 + 2`.
  pub fn eval_expr(&mut self, expr: &str) -> Result<Value, LangError> {
    let code = SourceCode::from_str(expr);
    let lexer = Lexer::new(&code);
    let mut parser = Parser::new(lexer);
    let expr = parser.parse_expression()?;
    let typed_expr = self.typechecker.with(code).analyze_expression(&expr)?;
    let value = self.interpreter.eval_expression(&typed_expr)?;

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
    let declarations = self.typechecker.with(code).analyze_statement(&statement)?;

    let mut opt_value = None;

    for decl in &declarations {
      opt_value = self.interpreter.eval_declaration(decl)?;

      if let Some(ty) = ast::declaration_type(decl) {
        self
          .typechecker
          .add_port(ast::declaration_name(decl), ty.clone());
      }
    }

    Ok(opt_value)
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
      self.load_analyzed_module(module_name)?;
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
        for (def, value) in &module.definitions {
          let name = if alias.is_empty() {
            def.clone()
          } else {
            format!("{alias}.{def}")
          };

          self.typechecker.add_port(&name, value.get_type());
          self.interpreter.stack.add(&name, value.clone());
        }

        Ok(())
      },
    )
  }

  fn load_analyzed_module(&mut self, module_name: &str) -> Result<(), LangError> {
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
    for dep in &dependencies {
      if !self.analyzed_modules.contains_key(dep) {
        self.load_analyzed_module(dep)?;
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
    let analyzed_module = typechecker.analyze_module(&self.analyzed_modules, module)?;

    self
      .analyzed_modules
      .insert(module_name.to_string(), analyzed_module);

    Ok(())
  }

  fn load_runtime_module(&mut self, module_name: &str) -> Result<(), LangError> {
    let dependencies = self
      .analyzed_modules
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
    for dep in &dependencies {
      if !self.runtime_modules.contains_key(dep) {
        self.load_runtime_module(dep)?;
      }
    }

    let mut interpreter = Interpreter::new();

    let runtime_module = {
      let module = self.analyzed_modules.get(module_name).ok_or_else(|| {
        LoaderError::MissingModule {
          module: module_name.to_string(),
        }
        .wrap()
      })?;

      interpreter.eval_module(&self.runtime_modules, module)?
    };

    let runtime_module = interpreter.eval_constants(runtime_module)?;

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
