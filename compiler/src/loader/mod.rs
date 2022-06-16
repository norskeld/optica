use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;
use std::sync::Arc;

use serde::{Deserialize, Serialize};

use crate::ast::untyped::Module;
use crate::ast::typed::{Declaration, Value};
use crate::errors::{LangError, LoaderError, Wrappable};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::runtime::Runtime;
use crate::source::{SourceCode, SourceFile};

#[derive(Clone, Debug)]
pub struct ModuleLoader;

#[derive(Clone, Debug)]
pub struct LoadedModule {
  pub src: SourceFile,
  pub ast: Module,
  pub dependencies: Vec<String>,
}

#[derive(Clone, Debug)]
pub struct AnalyzedModule {
  pub name: String,
  pub imports: Vec<ImportedModule>,
  pub declarations: Vec<Declaration>,
  pub dependencies: Vec<String>,
}

#[derive(Clone, Debug)]
pub struct RuntimeModule {
  pub name: String,
  pub imports: Vec<ImportedModule>,
  pub definitions: HashMap<String, Value>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct ImportedModule {
  pub source: String,
  pub source_name: String,
  pub destination_name: String,
}

impl ModuleLoader {
  pub fn include_folder(runtime: &mut Runtime, path: &str) -> Result<(), LangError> {
    let mut sources = vec![];
    let mut graph = HashMap::<String, Vec<String>>::new();
    let mut data = HashMap::<String, (SourceFile, Module)>::new();

    get_source_files(&mut sources, "", path)?;

    for source in sources {
      let ast = load_source_file(&source)?;

      graph.insert(source.name.to_string(), get_module_dependencies(&ast));
      data.insert(source.name.to_string(), (source, ast));
    }

    let keys = graph.keys().cloned().collect::<HashSet<_>>();

    for (_, value) in graph.iter_mut() {
      *value = value
        .iter()
        .filter(|dep| keys.contains(*dep))
        .cloned()
        .collect();
    }

    let checked = Self::check_for_cycles(graph)
      .map_err(|cycle| LoaderError::CyclicDependency { cycle }.wrap())?;

    for name in checked {
      let (source, ast) = data.remove(&name).unwrap();
      Self::include_module(runtime, source, ast)?;
    }

    Ok(())
  }

  pub fn include_file(
    runtime: &mut Runtime,
    inner_path: &str,
    path: &str,
  ) -> Result<(), LangError> {
    let source = get_source_file(inner_path, path)?;
    let ast = load_source_file(&source)?;

    Self::include_module(runtime, source, ast)
  }

  pub fn include_module(
    runtime: &mut Runtime,
    src: SourceFile,
    ast: Module,
  ) -> Result<(), LangError> {
    let dependencies = get_module_dependencies(&ast);

    let missing_deps = dependencies
      .iter()
      .filter(|dep| {
        !runtime.loaded_modules.contains_key(*dep) && !runtime.analyzed_modules.contains_key(*dep)
      })
      .cloned()
      .collect::<Vec<String>>();

    if !missing_deps.is_empty() {
      return Err(
        LoaderError::MissingDependencies {
          dependencies: missing_deps,
          src,
        }
        .wrap(),
      );
    }

    runtime.loaded_modules.insert(
      src.name.clone(),
      LoadedModule {
        src,
        ast,
        dependencies,
      },
    );

    Ok(())
  }

  fn check_for_cycles(graph: HashMap<String, Vec<String>>) -> Result<Vec<String>, Vec<String>> {
    let mut deps_graph: Vec<String> = Vec::new();
    let mut raw_graph = graph;

    while !raw_graph.is_empty() {
      let leaf = raw_graph
        .keys()
        .find(|key| raw_graph[*key].is_empty())
        .map(|x| x.to_string());

      match leaf {
        // No cycles detected, continuing.
        | Some(leaf) => {
          raw_graph.iter_mut().for_each(|(_, deps)| {
            while let Some(pos) = deps.iter().position(|x| x == &leaf) {
              deps.remove(pos);
            }
          });

          raw_graph.remove(&leaf);
          deps_graph.push(leaf);
        },
        // Cycle detected.
        | None => {
          let mut current: String = raw_graph.keys().next().unwrap().clone();
          let mut cycle: Vec<String> = vec![current.clone()];

          loop {
            let next = raw_graph[&current].first().unwrap().clone();

            if cycle.contains(&next) {
              cycle.push(next);
              break;
            } else {
              cycle.push(next.clone());
              current = next;
            }
          }

          return Err(cycle);
        },
      }
    }

    Ok(deps_graph)
  }
}

pub fn declaration_name(decl: &Declaration) -> &str {
  match decl {
    | Declaration::Alias(alias, ..) => &alias.name,
    | Declaration::Adt(name, ..) => name,
    | Declaration::Definition(name, ..) => name,
    | Declaration::Infix(name, ..) => name,
    | Declaration::Port(name, ..) => name,
  }
}

fn get_module_dependencies(module: &Module) -> Vec<String> {
  module
    .imports
    .iter()
    .map(|import| import.path.join("."))
    .collect::<Vec<String>>()
}

fn load_source_file(file: &SourceFile) -> Result<Module, LangError> {
  Parser::new(Lexer::new(&file.source)).parse_module()
}

fn get_source_files(
  destination: &mut Vec<SourceFile>,
  inner_path: &str,
  path: &str,
) -> Result<(), LangError> {
  let directory = fs::read_dir(path).map_err(|err| {
    LoaderError::IO {
      error: Arc::new(err),
      message: format!("read folder '{}'", path),
    }
    .wrap()
  })?;

  for entry_result in directory {
    let entry = entry_result.map_err(|err| {
      LoaderError::IO {
        error: Arc::new(err),
        message: "read folder entry".to_string(),
      }
      .wrap()
    })?;

    let file_type = entry.file_type().map_err(|err| {
      LoaderError::IO {
        error: Arc::new(err),
        message: "read file type".to_string(),
      }
      .wrap()
    })?;

    let file_name = entry.file_name().to_str().unwrap().to_string();
    let file_path = format!("{}/{}", path, file_name);

    if file_type.is_file() && file_name.ends_with(".cord") {
      destination.push(get_source_file(inner_path, &file_path)?);
    } else if file_type.is_dir() {
      let inner: String = if inner_path.is_empty() {
        file_name
      } else {
        format!("{}.{}", inner_path, file_name)
      };

      get_source_files(destination, &inner, &file_path)?
    }
  }

  Ok(())
}

fn get_source_file(inner_path: &str, absolute_path: &str) -> Result<SourceFile, LangError> {
  let path = Path::new(absolute_path);
  let file_name = path.file_name().unwrap().to_str().unwrap();

  let module_name = if inner_path.is_empty() {
    file_name.to_string()
  } else {
    format!("{}.{}", inner_path, file_name)
  };

  let file_contents = fs::read(absolute_path).map_err(|err| {
    LoaderError::IO {
      error: Arc::new(err),
      message: format!("read file '{}'", absolute_path),
    }
    .wrap()
  })?;

  let loaded_module = SourceFile {
    name: module_name.trim_end_matches(".cord").to_string(),
    path: absolute_path.to_string(),
    source: SourceCode::from_bytes(file_contents, absolute_path),
  };

  Ok(loaded_module)
}
