use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use crate::ast;
use crate::ast::untyped::{
  AdtExports, Definition, Expression, ModuleExport, ModuleExports, ModuleImport, Pattern,
  Statement, Type, TypeAlias,
};
use crate::ast::typed::{
  Adt, AdtVariant, Declaration, TypedDefinition, TypedExpression, TypedPattern, Value,
};
use crate::ast::traverser;
use crate::errors::{InterpreterError, LangError, LoaderError, TypeError, Wrappable};
use crate::source::{SourceCode, Span};
use crate::loader::{self, AnalyzedModule, ImportedModule, LoadedModule};
use crate::utils::vec::{self, VecExt};
use crate::utils::path;
use super::Context;
use super::fold::{self, ExpressionTreeError};
use super::helpers;

#[derive(Debug, Clone)]
struct Constraint {
  span: Span,
  left: Type,
  right: Type,
}

impl Constraint {
  fn new(span: Span, left: &Type, right: &Type) -> Self {
    Constraint {
      span,
      left: left.clone(),
      right: right.clone(),
    }
  }

  fn as_pair(&self) -> (&Type, &Type) {
    (&self.left, &self.right)
  }
}

#[derive(Debug)]
struct Substitution(HashMap<Type, Type>);

impl Substitution {
  fn empty() -> Self {
    Substitution(HashMap::new())
  }

  fn var_pair(var: &str, ty: &Type) -> Self {
    Substitution(HashMap::from([(Type::Var(var.to_string()), ty.clone())]))
  }

  fn merge(self, sbst: &Substitution) -> Substitution {
    let mut map = HashMap::new();

    let Substitution(this) = self;
    let Substitution(that) = sbst;

    this.into_iter().for_each(|(key, value)| {
      map.extend([(key, apply_substitution_ty(sbst, &value))].into_iter())
    });

    that
      .iter()
      .for_each(|(key, value)| map.extend([(key.to_owned(), value.to_owned())].into_iter()));

    Substitution(map)
  }

  fn replace(&self, ty: Type) -> Type {
    let Substitution(this) = self;

    this.get(&ty).cloned().unwrap_or(ty)
  }
}

#[derive(Debug)]
pub struct Typechecker {
  pub context: Context,
  source: SourceCode,
}

impl Typechecker {
  pub fn new(source: SourceCode) -> Self {
    Typechecker {
      context: Context::new(),
      source,
    }
  }

  pub fn with(&self, source: SourceCode) -> Self {
    Typechecker {
      context: self.context.clone(),
      source,
    }
  }

  pub fn add_port(&mut self, name: &str, ty: Type) {
    self.context.set(name, ty);
  }

  pub fn add_type_alias(&mut self, alias: TypeAlias) {
    self.context.set_type_alias(alias);
  }

  pub fn add_canonical_type_name(&mut self, name: &str, canonical: &str) {
    self
      .context
      .set_canonical_type_name(name, canonical.to_string());
  }

  pub fn analyze_statement(&mut self, stmt: &Statement) -> Result<Vec<Declaration>, LangError> {
    let statements = match stmt {
      | Statement::Alias(name, vars, ty) => self.analyze_statement_typealias(name, vars, ty)?,
      | Statement::Adt(name, vars, variants) => self.analyze_statement_adt(name, vars, variants)?,
      | Statement::Function(func) => self.analyze_statement_definition(func)?,
      | Statement::Port(span, name, ty) => self.analyze_statement_port(*span, name, ty)?,
      | Statement::Infix(_, _, name, def) => {
        if let Some(ty) = self.context.get(name) {
          vec![Declaration::Infix(name.clone(), def.clone(), ty.clone())]
        } else if let Some(ty) = self.context.get(def) {
          vec![Declaration::Infix(name.clone(), def.clone(), ty.clone())]
        } else {
          vec![]
        }
      },
    };

    Ok(statements)
  }

  pub fn analyze_module(
    &mut self,
    modules: &HashMap<String, AnalyzedModule>,
    module: &LoadedModule,
  ) -> Result<AnalyzedModule, LangError> {
    let imports = self.analyze_module_imports(modules, &module.ast.imports)?;

    // Avoid problems with statement order.
    for stmt in &module.ast.statements {
      if let Some(ty) = declared_statement_type(stmt) {
        self
          .context
          .set(declared_statement_name(stmt).unwrap(), ty.clone());
      }
    }

    // Custom behavior for binary operators.
    for statement in &module.ast.statements {
      if let Statement::Infix(_, _, name, def) = statement {
        let ty = if let Some(ty) = self.context.get(def) {
          ty.clone()
        } else {
          unreachable!(
            "Infix operator {} where the function {} doesn't have a type header",
            name, def
          );
        };

        self.context.set(name, ty);
      }
    }

    let all_declarations = self
      .analyze_module_declarations(&module.ast.statements)
      .map_err(|list| {
        if list.len() == 1 {
          list.into_iter().next().unwrap()
        } else {
          LangError::List(list)
        }
      })?;

    Ok(AnalyzedModule {
      name: module.src.name.to_string(),
      dependencies: module.dependencies.clone(),
      declarations: all_declarations,
      imports,
    })
  }
}

impl Typechecker {
  pub fn analyze_statement_typealias(
    &mut self,
    name: &str,
    decl_vars: &[String],
    ty: &Type,
  ) -> Result<Vec<Declaration>, LangError> {
    let mut used_vars: HashSet<String> = HashSet::new();

    traverser::traverse_type(&mut used_vars, ty, &|set, node| {
      if let Type::Var(var) = &node {
        set.insert(var.clone());
      }
    });

    if used_vars.len() < decl_vars.len() {
      let unused_vars = decl_vars
        .iter()
        .filter(|t| !used_vars.contains(*t))
        .cloned()
        .collect::<Vec<String>>();

      return Err(LangError::Typechecker(
        self.source.clone(),
        TypeError::UnusedTypeVariables {
          name: name.to_string(),
          values: unused_vars,
        },
      ));
    }

    if used_vars.len() > decl_vars.len() {
      let unknown_vars = used_vars
        .into_iter()
        .filter(|t| !decl_vars.contains(t))
        .collect::<Vec<String>>();

      return Err(LangError::Typechecker(
        self.source.clone(),
        TypeError::UndeclaredTypeVariables {
          name: name.to_string(),
          values: unknown_vars,
        },
      ));
    }

    let decls: Vec<Declaration> = vec![Declaration::Alias(TypeAlias {
      name: name.to_string(),
      variables: decl_vars.to_owned(),
      replacement: ty.clone(),
    })];

    Ok(decls)
  }

  pub fn analyze_statement_adt(
    &mut self,
    name: &str,
    decl_vars: &[String],
    variants: &[(Span, String, Vec<Type>)],
  ) -> Result<Vec<Declaration>, LangError> {
    let vars: Vec<Type> = decl_vars.iter().map(|var| Type::Var(var.into())).collect();
    let adt_type = Type::Tag(name.into(), vars);

    let mut decls = vec![];

    // Any error inside the block should be returned after `exit_block()`. We cannot use
    // `self.env.block()`, because we call `self.check_type()`, it needs an immutable reference to
    // self and `self.env.block()` already has a mutable reference to self.
    self.context.enter_block();

    let adt_variants = {
      // Register own name to allow recursive definitions.
      self.add_canonical_type_name(name, name);

      let mut adt_variants: Vec<(Span, AdtVariant)> = vec![];

      for (span, name, types) in variants {
        let mut new_types = vec![];

        for ty in types {
          new_types.push(self.check_type(*span, ty.clone())?);
        }

        adt_variants.push((
          *span,
          AdtVariant {
            name: name.clone(),
            types: new_types,
          },
        ));
      }

      Ok(adt_variants)
    };

    self.context.exit_block();

    // Return if Err(_).
    let adt_variants: Vec<(Span, AdtVariant)> = adt_variants?;

    // For each variant a definition is added, this definition is a constructor.
    for (span, variant) in &adt_variants {
      let mut new_variant_types = vec![];

      for ty in &variant.types {
        new_variant_types.push(self.check_type(*span, ty.clone())?);
      }

      let variant_type = if !new_variant_types.is_empty() {
        helpers::build_func_type(&vec::create_vec_inv(&new_variant_types, adt_type.clone()))
      } else {
        adt_type.clone()
      };

      decls.push(Declaration::Port(variant.name.clone(), variant_type));
    }

    let adt = Arc::new(Adt {
      name: name.to_owned(),
      types: decl_vars.to_owned(),
      variants: adt_variants.map(|(_, a)| a.clone()),
    });

    decls.push(Declaration::Adt(name.to_owned(), adt));

    Ok(decls)
  }

  pub fn analyze_statement_port(
    &mut self,
    span: Span,
    name: &str,
    ty: &Type,
  ) -> Result<Vec<Declaration>, LangError> {
    let checked_type = self.check_type(span, ty.clone())?;

    Ok(vec![Declaration::Port(name.to_owned(), checked_type)])
  }

  pub fn analyze_statement_definition(
    &mut self,
    function: &Definition,
  ) -> Result<Vec<Declaration>, LangError> {
    let typed_function = self.analyze_definition(function)?;

    Ok(vec![Declaration::Definition(
      function.name.clone(),
      typed_function,
    )])
  }
}

impl Typechecker {
  pub fn analyze_definition(&mut self, func: &Definition) -> Result<TypedDefinition, LangError> {
    infer_definition_type(&mut self.context, func)
      .map_err(|error| LangError::Typechecker(self.source.clone(), error))
  }

  pub fn analyze_expression(&mut self, expr: &Expression) -> Result<TypedExpression, LangError> {
    infer_expression_type(&mut self.context, expr)
      .map_err(|e| LangError::Typechecker(self.source.clone(), e))
  }

  pub fn check_type(&self, span: Span, ty: Type) -> Result<Type, LangError> {
    self
      .get_canonical_type(span, self.replace_type_alias(ty))
      .map_err(|e| LangError::Typechecker(self.source.clone(), e))
  }

  pub fn get_canonical_type(&self, span: Span, ty: Type) -> Result<Type, TypeError> {
    let next_ty = match ty {
      | Type::Unit => Type::Unit,
      | Type::Var(_) => ty,
      | Type::Tag(name, items) => {
        let canonical_name = match self.context.get_canonical_type_name(&name) {
          | Some(name_str) => name_str.to_string(),
          | None => {
            return Err(TypeError::UnknownType {
              span,
              name: name.clone(),
            });
          },
        };

        Type::Tag(
          canonical_name,
          items
            .into_iter()
            .map(|it| self.get_canonical_type(span, it))
            .collect::<Result<Vec<Type>, TypeError>>()?,
        )
      },
      | Type::Function(a, b) => Type::Function(
        Box::new(self.get_canonical_type(span, *a)?),
        Box::new(self.get_canonical_type(span, *b)?),
      ),
      | Type::Tuple(items) => Type::Tuple(
        items
          .into_iter()
          .map(|it| self.get_canonical_type(span, it))
          .collect::<Result<Vec<Type>, TypeError>>()?,
      ),
    };

    Ok(next_ty)
  }

  pub fn replace_type_alias(&self, ty: Type) -> Type {
    match ty {
      | Type::Tag(a, b) => {
        let params: Vec<Type> = b
          .into_iter()
          .map(|it| self.replace_type_alias(it))
          .collect();

        if let Some(alias) = self.context.get_type_alias(&a) {
          assert_eq!(params.len(), alias.variables.len());

          let mut map = HashMap::new();

          for (index, param) in params.iter().enumerate() {
            map.insert(Type::Var(alias.variables[index].clone()), param.clone());
          }

          let sub = Substitution(map);
          let new_params: Vec<Type> = params.into_iter().map(|it| sub.replace(it)).collect();

          Type::Tag(a, new_params)
        } else {
          Type::Tag(a, params)
        }
      },
      | Type::Unit => Type::Unit,
      | Type::Var(a) => Type::Var(a),
      | Type::Function(a, b) => Type::Function(
        Box::new(self.replace_type_alias(*a)),
        Box::new(self.replace_type_alias(*b)),
      ),
      | Type::Tuple(a) => Type::Tuple(
        a.into_iter()
          .map(|it| self.replace_type_alias(it))
          .collect(),
      ),
    }
  }
}

impl Typechecker {
  pub fn analyze_module_imports(
    &mut self,
    modules: &HashMap<String, AnalyzedModule>,
    imports: &[ModuleImport],
  ) -> Result<Vec<ImportedModule>, LangError> {
    let mut module_imports = vec![ImportedModule {
      source: "Cord.Kernel.Basics".to_string(),
      source_name: "__internal__minus".to_string(),
      destination_name: "__internal__minus".to_string(),
    }];

    self.add_port("__internal__minus", ast::type_unary_minus());

    // Add rest of imports.
    for import in imports {
      self.analyze_import(modules, &mut module_imports, import)?;
    }

    Ok(module_imports)
  }

  fn import_qualified(
    &mut self,
    module_name: &str,
    alias: &str,
    module: &AnalyzedModule,
    result: &mut Vec<ImportedModule>,
  ) {
    for declaration in &module.declarations {
      self.import_declaration(declaration, module_name, alias, result);
    }
  }

  fn import_exports(
    &mut self,
    module_name: &str,
    module: &AnalyzedModule,
    exports: &ModuleExports,
    result: &mut Vec<ImportedModule>,
  ) -> Result<(), LangError> {
    let declarations = match exports {
      | ModuleExports::Just(exp) => {
        Self::get_exposed_decls(&module.declarations, exp).map_err(Wrappable::wrap)?
      },
      | ModuleExports::All => module.declarations.clone(),
    };

    for declaration in &declarations {
      self.import_declaration(declaration, module_name, "", result);
    }

    Ok(())
  }

  fn import_declaration(
    &mut self,
    declaration: &Declaration,
    module_name: &str,
    alias: &str,
    result: &mut Vec<ImportedModule>,
  ) {
    let aliased_name = if alias.is_empty() {
      loader::declaration_name(declaration).to_string()
    } else {
      format!("{}.{}", alias, loader::declaration_name(declaration))
    };

    match declaration {
      | Declaration::Port(name, ty) => {
        result.push(ImportedModule {
          source: module_name.to_string(),
          source_name: name.clone(),
          destination_name: aliased_name.clone(),
        });

        self.add_port(&aliased_name, ty.clone());
      },
      | Declaration::Definition(name, def) => {
        result.push(ImportedModule {
          source: module_name.to_string(),
          source_name: name.clone(),
          destination_name: aliased_name.clone(),
        });

        self.add_port(&aliased_name, def.header.clone());
      },
      | Declaration::Alias(alias) => {
        self.add_type_alias(alias.clone());
      },
      | Declaration::Adt(name, _) => {
        self.add_canonical_type_name(&aliased_name, name);
        self.add_canonical_type_name(name, name);
      },
      | Declaration::Infix(name, _, ty) => {
        result.push(ImportedModule {
          source: module_name.to_string(),
          source_name: name.clone(),
          destination_name: name.clone(),
        });

        self.add_port(name, ty.clone());
      },
    }
  }

  fn analyze_import(
    &mut self,
    modules: &HashMap<String, AnalyzedModule>,
    module_imports: &mut Vec<ImportedModule>,
    import: &ModuleImport,
  ) -> Result<(), LangError> {
    let module_name = import.path.join(".");

    let module = modules.get(&module_name).ok_or_else(|| {
      LoaderError::MissingModule {
        module: module_name.clone(),
      }
      .wrap()
    })?;

    let alias = import.alias.as_ref().unwrap_or(&module_name);

    self.import_qualified(&module_name, alias, module, module_imports);

    if let Some(exposing) = &import.exports {
      self.import_exports(&module_name, module, exposing, module_imports)?;
    }

    Ok(())
  }

  pub fn analyze_module_declarations(
    &mut self,
    statements: &[Statement],
  ) -> Result<Vec<Declaration>, Vec<LangError>> {
    let mut statements = statements.iter().collect::<Vec<_>>();

    // Sort by type.
    statements.sort_by_key(|stmt| match *stmt {
      | Statement::Adt(_, _, _) => 1,
      | Statement::Alias(_, _, _) => 2,
      | Statement::Port(_, _, _) => 3,
      | Statement::Infix(_, _, _, _) => 4,
      | Statement::Function(_) => 5,
    });

    let mut declarations = vec![];
    let mut errors = vec![];

    for stmt in statements {
      let decls = match self.analyze_statement(stmt) {
        | Ok(decls) => decls,
        | Err(err) => {
          errors.push(err);
          vec![]
        },
      };

      for decl in decls.into_iter() {
        declarations.push(decl.clone());

        match decl {
          | Declaration::Definition(name, def) => {
            self.add_port(&name, def.header.clone());
          },
          | Declaration::Port(name, ty) => {
            self.add_port(&name, ty.clone());
          },
          | Declaration::Alias(alias) => {
            self.add_type_alias(alias.clone());
          },
          | Declaration::Adt(name, adt) => {
            self.add_canonical_type_name(&name, &adt.name);
          },
          | Declaration::Infix(name, _, ty) => {
            self.add_port(&name, ty.clone());
          },
        }
      }
    }

    // Replace infix definitions with copies of the referenced function.
    for decl in declarations.clone() {
      if let Declaration::Infix(name, infix_def, _) = decl {
        let mut new_declaration = vec![];

        for declaration in &declarations {
          if let Declaration::Definition(def_name, def) = declaration {
            if def_name == &infix_def {
              let mut def = def.clone();

              def.name = name.to_string();
              new_declaration.push(Declaration::Definition(name.to_string(), def));

              break;
            }
          }
        }

        declarations.extend(new_declaration);
      }
    }

    if errors.is_empty() {
      Ok(declarations)
    } else {
      Err(errors)
    }
  }

  fn get_exposed_decls(
    all_declarations: &[Declaration],
    exposed: &[ModuleExport],
  ) -> Result<Vec<Declaration>, InterpreterError> {
    let mut exposed_declarations = Vec::new();

    for exp in exposed.iter() {
      match exp {
        | ModuleExport::Adt(name, adt_exp) => {
          match adt_exp {
            | AdtExports::Just(variants) => {
              for it in all_declarations.iter() {
                let (variant_name, ty) = match it {
                  | Declaration::Definition(variant_name, def) => (variant_name, &def.header),
                  | Declaration::Port(variant_name, ty) => (variant_name, ty),
                  | _ => continue,
                };

                if variants.contains(variant_name) {
                  if let Type::Tag(tag_name, _) = Self::get_func_return(ty) {
                    if &tag_name == name {
                      exposed_declarations.push(it.clone());
                    }
                  }
                }
              }
            },
            | AdtExports::All => {
              for declaration in all_declarations.iter() {
                let ty = match declaration {
                  | Declaration::Definition(_, def) => &def.header,
                  | Declaration::Port(_, ty) => ty,
                  | _ => continue,
                };

                if let Type::Tag(tag_name, _) = Self::get_func_return(ty) {
                  if &tag_name == name {
                    exposed_declarations.push(declaration.clone());
                  }
                }
              }
            },
          }

          let declaration = all_declarations
            .iter()
            .find(|decl| {
              if let Declaration::Adt(adt_name, _) = decl {
                adt_name == name
              } else {
                false
              }
            })
            .cloned()
            .ok_or_else(|| {
              InterpreterError::MissingExposing(name.clone(), all_declarations.to_owned())
            })?;

          exposed_declarations.push(declaration);
        },
        | ModuleExport::Type(name) => {
          let declaration = all_declarations
            .iter()
            .find(|decl| {
              if let Declaration::Alias(alias) = decl {
                &alias.name == name
              } else if let Declaration::Adt(adt_name, _) = decl {
                adt_name == name
              } else {
                false
              }
            })
            .cloned()
            .ok_or_else(|| {
              InterpreterError::MissingExposing(name.clone(), all_declarations.to_owned())
            })?;

          exposed_declarations.push(declaration);
        },
        | ModuleExport::BinaryOperator(name) => {
          let declaration = all_declarations
            .iter()
            .find(|decl| {
              if let Declaration::Definition(def_name, _) = decl {
                def_name == name
              } else {
                false
              }
            })
            .cloned();

          if let Some(decl) = declaration {
            exposed_declarations.push(decl);
          }
        },
        | ModuleExport::Function(name) => {
          let declaration = all_declarations
            .iter()
            .find(|decl| {
              if let Declaration::Definition(def_name, _) = decl {
                def_name == name
              } else {
                false
              }
            })
            .cloned()
            .ok_or_else(|| {
              InterpreterError::MissingExposing(name.clone(), all_declarations.to_owned())
            })?;

          exposed_declarations.push(declaration);
        },
      }
    }

    Ok(exposed_declarations)
  }

  fn get_func_return(ty: &Type) -> Type {
    if let Type::Function(_, ty) = ty {
      Self::get_func_return(&*ty)
    } else {
      ty.clone()
    }
  }
}

fn declared_statement_type(stmt: &Statement) -> Option<&Type> {
  match stmt {
    | Statement::Alias(_, _, _) => None,
    | Statement::Adt(_, _, _) => None,
    | Statement::Infix(_, _, _, _) => None,
    | Statement::Port(_, _, ty) => Some(ty),
    | Statement::Function(func) => {
      if let Some(ty) = &func.header {
        Some(ty)
      } else {
        None
      }
    },
  }
}

fn declared_statement_name(stmt: &Statement) -> Option<&str> {
  match stmt {
    | Statement::Alias(_, _, _) => None,
    | Statement::Adt(_, _, _) => None,
    | Statement::Infix(_, _, name, _) => Some(name),
    | Statement::Port(_, name, _) => Some(name),
    | Statement::Function(func) => Some(&func.name),
  }
}

fn unpack_types(ty: &Type) -> Vec<Type> {
  let mut current = ty.clone();
  let mut components = vec![];

  while let Type::Function(a, b) = current {
    components.push((*a).clone());
    current = (*b).clone();
  }

  components.push(current.clone());
  components
}

fn infer_expression_type(
  env: &mut Context,
  expr: &Expression,
) -> Result<TypedExpression, TypeError> {
  let mut constraints = vec![];

  let (substitution, annotated_expr) = env.block(|env| {
    // Type annotation.
    let annotated_expr = annotate_expr(env, expr)?;

    // Collect constraints.
    collect_expr_constraints(&mut constraints, &annotated_expr);

    // Constraint solutions.
    let substitution = match unify_constraints(&constraints) {
      | Ok(sub) => sub,
      | Err(e) => {
        return Err(e);
      },
    };

    Ok((substitution, annotated_expr))
  })?;

  // Apply solution.
  let res_expr = replace_expr_types(&substitution, annotated_expr);

  Ok(res_expr)
}

fn infer_definition_type(
  env: &mut Context,
  func: &Definition,
) -> Result<TypedDefinition, TypeError> {
  let mut constraints = vec![];

  // Exhaustive patterns?
  let (substitution, func_type, annotated_patterns, annotated_expr) = env.block(|env| {
    // Type Annotation.
    let func_type = env.next_type();

    env.set(&func.name, func_type.clone());

    let mut annotated_patterns = vec![];

    for pattern in &func.patterns {
      annotated_patterns.push(annotate_pattern(env, pattern)?);
    }

    for pattern in &annotated_patterns {
      add_pattern_vars_to_env(env, pattern);
    }

    let annotated_expr = annotate_expr(env, &func.expression)?;

    // Collect constraints.
    if let Some(ty) = &func.header {
      let safe_ty = update_type_variables(env, &mut HashMap::new(), ty.clone());

      collect_type_definition_constraints(
        &mut constraints,
        &safe_ty,
        &annotated_patterns,
        &annotated_expr,
      );
    };

    for pat in &annotated_patterns {
      collect_pattern_constraints(&mut constraints, pat);
    }

    collect_expr_constraints(&mut constraints, &annotated_expr);

    // Function type.
    let mut func_types: Vec<Type> = annotated_patterns
      .iter()
      .map(|pattern| pattern.get_type())
      .collect();

    func_types.push(annotated_expr.get_type());

    constraints.push(Constraint::new(
      annotated_expr.get_span(),
      &func_type,
      &ast::type_func(func_types),
    ));

    // Constraint solutions.
    let substitution = match unify_constraints(&constraints) {
      | Ok(sub) => sub,
      | Err(e) => {
        return Err(e);
      },
    };

    Ok((substitution, func_type, annotated_patterns, annotated_expr))
  })?;

  // Apply solution.
  let res_expr = replace_expr_types(&substitution, annotated_expr);
  let res_patterns: Vec<TypedPattern> = annotated_patterns
    .into_iter()
    .map(|pat| replace_pattern_types(&substitution, pat))
    .collect();

  let def_type = substitution.replace(func_type);

  Ok(TypedDefinition {
    header: def_type,
    name: func.name.to_string(),
    patterns: res_patterns,
    expression: res_expr,
  })
}

fn update_type_variables(env: &mut Context, map: &mut HashMap<String, Type>, ty: Type) -> Type {
  match ty {
    | Type::Unit => Type::Unit,
    | Type::Var(name) => match map.get(&name).cloned() {
      | Some(var) => var,
      | None => {
        let new_ty = if name.starts_with("comparable") {
          env.next_comparable_type()
        } else if name.starts_with("appendable") {
          env.next_appendable_type()
        } else if name.starts_with("number") {
          env.next_number_type()
        } else {
          env.next_type()
        };

        map.insert(name, new_ty.clone());
        new_ty
      },
    },
    | Type::Function(a, b) => Type::Function(
      Box::new(update_type_variables(env, map, *a)),
      Box::new(update_type_variables(env, map, *b)),
    ),
    | Type::Tag(name, items) => {
      let vec: Vec<Type> = items
        .into_iter()
        .map(|e| update_type_variables(env, map, e))
        .collect();

      Type::Tag(name, vec)
    },
    | Type::Tuple(items) => {
      let vec: Vec<Type> = items
        .into_iter()
        .map(|e| update_type_variables(env, map, e))
        .collect();

      Type::Tuple(vec)
    },
  }
}

fn annotate_pattern(env: &mut Context, pattern: &Pattern) -> Result<TypedPattern, TypeError> {
  let typed = match pattern {
    | Pattern::Var(span, name) => {
      if env.get(name).is_some() {
        return Err(TypeError::VariableNameShadowed {
          span: pattern.span(),
          name: name.clone(),
        });
      }

      TypedPattern::Var(*span, env.next_type(), name.clone())
    },
    | Pattern::Adt(span, name, items) => {
      let adt_type = match env.get(name) {
        | Some(ty) => ty.clone(),
        | None => {
          return Err(TypeError::MissingDefinition {
            span: *span,
            name: name.to_string(),
          });
        },
      };

      let adt_type = update_type_variables(env, &mut HashMap::new(), adt_type);

      TypedPattern::Adt(
        *span,
        env.next_type(),
        adt_type,
        items
          .iter()
          .map(|e| annotate_pattern(env, e))
          .collect::<Result<_, _>>()?,
      )
    },
    | Pattern::Wildcard(span) => TypedPattern::Wildcard(*span),
    | Pattern::Unit(span) => TypedPattern::Unit(*span),
    | Pattern::Tuple(span, items) => TypedPattern::Tuple(
      *span,
      env.next_type(),
      items
        .iter()
        .map(|e| annotate_pattern(env, e))
        .collect::<Result<_, _>>()?,
    ),
    | Pattern::List(span, items) => TypedPattern::List(
      *span,
      env.next_type(),
      items
        .iter()
        .map(|e| annotate_pattern(env, e))
        .collect::<Result<_, _>>()?,
    ),
    | Pattern::BinaryOperator(span, op, a, b) => TypedPattern::BinaryOperator(
      *span,
      env.next_type(),
      op.clone(),
      Box::new(annotate_pattern(env, a)?),
      Box::new(annotate_pattern(env, b)?),
    ),
    | Pattern::LitInt(span, val) => TypedPattern::LitInt(*span, *val),
    | Pattern::LitString(span, val) => TypedPattern::LitString(*span, val.clone()),
    | Pattern::LitChar(span, val) => TypedPattern::LitChar(*span, *val),
    | Pattern::Alias(span, pat, name) => {
      let ty = annotate_pattern(env, pat)?;
      env.set(name, ty.get_type());

      TypedPattern::Alias(*span, ty.get_type(), Box::new(ty), name.clone())
    },
  };

  Ok(typed)
}

fn annotate_expr(env: &mut Context, expr: &Expression) -> Result<TypedExpression, TypeError> {
  let te = match expr {
    | Expression::QualifiedRef(span, base, name) => {
      let name = path::qualified_name(base, name);

      let ty = env
        .get(&name)
        .cloned()
        .ok_or_else(|| TypeError::MissingDefinition {
          span: *span,
          name: name.to_string(),
        })?;

      let ty = update_type_variables(env, &mut HashMap::new(), ty);

      TypedExpression::Ref(*span, ty, name)
    },
    | Expression::Ref(span, name) => {
      let ty = env
        .get(name)
        .cloned()
        .ok_or_else(|| TypeError::MissingDefinition {
          span: *span,
          name: name.to_string(),
        })?;

      let ty = if let Type::Var(_) = &ty {
        ty
      } else {
        update_type_variables(env, &mut HashMap::new(), ty)
      };

      TypedExpression::Ref(*span, ty, name.clone())
    },
    | Expression::Literal(span, lit) => {
      let value: Value = lit.clone().into();

      if let Value::Number(_) = &value {
        TypedExpression::Const(*span, env.next_number_type(), value)
      } else {
        TypedExpression::Const(*span, value.get_type(), value)
      }
    },
    | Expression::Unit(span) => TypedExpression::Const(*span, env.next_type(), Value::Unit),
    | Expression::Tuple(span, exprs) => {
      let exprs = exprs
        .iter()
        .map(|e| annotate_expr(env, e))
        .collect::<Result<Vec<_>, TypeError>>()?;

      TypedExpression::Tuple(*span, env.next_type(), exprs)
    },
    | Expression::List(span, exprs) => {
      let exprs = exprs
        .iter()
        .map(|e| annotate_expr(env, e))
        .collect::<Result<Vec<_>, TypeError>>()?;

      TypedExpression::List(*span, env.next_type(), exprs)
    },
    | Expression::If(span, cond_expr, then_expr, else_expr) => TypedExpression::If(
      *span,
      env.next_type(),
      Box::new(annotate_expr(env, cond_expr)?),
      Box::new(annotate_expr(env, then_expr)?),
      Box::new(annotate_expr(env, else_expr)?),
    ),
    | Expression::Lambda(span, patterns, expr) => {
      let patterns = patterns
        .iter()
        .map(|pattern| annotate_pattern(env, pattern))
        .collect::<Result<Vec<_>, TypeError>>()?;

      let expr = env.block(|env| {
        for pattern in &patterns {
          add_pattern_vars_to_env(env, pattern);
        }

        annotate_expr(env, expr)
      })?;

      TypedExpression::Lambda(*span, env.next_type(), patterns, Box::new(expr))
    },
    | Expression::Application(span, a, b) => TypedExpression::Application(
      *span,
      env.next_type(),
      Box::new(annotate_expr(env, a)?),
      Box::new(annotate_expr(env, b)?),
    ),
    | Expression::OperatorChain(span, exprs, ops) => match fold::create_expression_tree(exprs, ops)
    {
      | Ok(tree) => annotate_expr(env, &fold::to_expression(tree))?,
      | Err(err) => {
        let message = match err {
          | ExpressionTreeError::InvalidInput => "Invalid input".to_string(),
          | ExpressionTreeError::AssociativityError => "Associativity error".to_string(),
          | ExpressionTreeError::InternalError(msg) => format!("Internal error: {}", msg),
        };

        return Err(TypeError::InvalidOperandChain {
          span: *span,
          message,
        });
      },
    },
  };

  Ok(te)
}

fn collect_type_definition_constraints(
  res: &mut Vec<Constraint>,
  ty: &Type,
  patterns: &[TypedPattern],
  expr: &TypedExpression,
) {
  let list = unpack_types(ty);

  if list.len() <= patterns.len() {
    panic!(
      "Too many patterns: {} patterns and {} arguments",
      patterns.len(),
      list.len()
    );
  }

  let mut index = 0;

  while index < patterns.len() {
    res.push(Constraint::new(
      patterns[index].get_span(),
      &patterns[index].get_type(),
      &list[index],
    ));

    index += 1;
  }

  let ret: Vec<_> = list[index..].to_vec();

  res.push(Constraint::new(
    expr.get_span(),
    &ast::type_func(ret),
    &expr.get_type(),
  ));
}

fn collect_pattern_constraints(res: &mut Vec<Constraint>, pat: &TypedPattern) {
  match pat {
    | TypedPattern::Var(_, _, _) => {},
    | TypedPattern::Adt(_, ty, ctor_type, items) => {
      let adt_type = unpack_types(ctor_type).into_iter().last().unwrap();

      let mut ctor = vec![];

      for arg in items {
        ctor.push(arg.get_type());
      }

      ctor.push(adt_type.clone());

      res.push(Constraint::new(
        pat.get_span(),
        ctor_type,
        &ast::type_func(ctor),
      ));

      res.push(Constraint::new(pat.get_span(), ty, &adt_type));
      items.for_each(|it| collect_pattern_constraints(res, it));
    },
    | TypedPattern::Wildcard(_) => {},
    | TypedPattern::Unit(_) => {},
    | TypedPattern::Tuple(_, ty, items) => {
      res.push(Constraint::new(
        pat.get_span(),
        ty,
        &Type::Tuple(items.map(|e| e.get_type())),
      ));

      items.for_each(|it| collect_pattern_constraints(res, it));
    },
    | TypedPattern::List(_, ty, items) => {
      items.for_each(|it| {
        res.push(Constraint::new(
          pat.get_span(),
          ty,
          &ast::type_list(it.get_type()),
        ));

        collect_pattern_constraints(res, it);
      });
    },
    | TypedPattern::BinaryOperator(_, ty, op, a, b) => {
      assert_eq!("::", op.as_str());

      res.push(Constraint::new(
        pat.get_span(),
        ty,
        &ast::type_list(a.get_type()),
      ));

      res.push(Constraint::new(
        pat.get_span(),
        &b.get_type(),
        &ast::type_list(a.get_type()),
      ));

      collect_pattern_constraints(res, a);
      collect_pattern_constraints(res, b);
    },
    | TypedPattern::LitInt(_, _) => {},
    | TypedPattern::LitString(_, _) => {},
    | TypedPattern::LitChar(_, _) => {},
    | TypedPattern::Alias(_, _, p, _) => {
      collect_pattern_constraints(res, p);
    },
  }
}

fn collect_expr_constraints(res: &mut Vec<Constraint>, expr: &TypedExpression) {
  match expr {
    | TypedExpression::Ref(_, _, _) => { /* ignore */ },
    | TypedExpression::Const(_, _, _) => { /* ignore */ },
    | TypedExpression::Tuple(_, ty, exprs) => {
      res.push(Constraint::new(
        expr.get_span(),
        ty,
        &Type::Tuple(exprs.map(|expr| expr.get_type())),
      ));

      for expr in exprs {
        collect_expr_constraints(res, expr);
      }
    },
    | TypedExpression::List(_, ty, exprs) => {
      for expr in exprs {
        res.push(Constraint::new(
          expr.get_span(),
          ty,
          &ast::type_list(expr.get_type()),
        ));

        collect_expr_constraints(res, expr);
      }
    },
    | TypedExpression::If(_, ty, a, b, c) => {
      res.push(Constraint::new(
        expr.get_span(),
        &a.get_type(),
        &ast::type_bool(),
      ));

      res.push(Constraint::new(expr.get_span(), ty, &b.get_type()));
      res.push(Constraint::new(expr.get_span(), ty, &c.get_type()));

      collect_expr_constraints(res, a);
      collect_expr_constraints(res, b);
      collect_expr_constraints(res, c);
    },
    | TypedExpression::Case(span, ty, expr, cases) => {
      collect_expr_constraints(res, expr);

      for (pattern, expression) in cases {
        collect_pattern_constraints(res, pattern);
        collect_expr_constraints(res, expression);

        res.push(Constraint::new(*span, ty, &expression.get_type()));
      }
    },
    | TypedExpression::Lambda(span, ty, patterns, expr) => {
      let mut chain = vec![];

      for pattern in patterns {
        chain.push(pattern.get_type());
        collect_pattern_constraints(res, pattern);
      }

      chain.push(expr.get_type());
      res.push(Constraint::new(*span, ty, &ast::type_func(chain)));

      collect_expr_constraints(res, expr);
    },
    | TypedExpression::Application(_, ty, a, b) => {
      res.push(Constraint::new(
        expr.get_span(),
        &a.get_type(),
        &Type::Function(Box::new(b.get_type()), Box::new(ty.clone())),
      ));

      collect_expr_constraints(res, a);
      collect_expr_constraints(res, b);
    },
    | TypedExpression::Let(_, _, _, expr) => {
      collect_expr_constraints(res, expr);
    },
  }
}

fn unify_constraints(constraints: &[Constraint]) -> Result<Substitution, TypeError> {
  if constraints.is_empty() {
    return Ok(Substitution::empty());
  }

  let mut sub = Substitution::empty();
  let mut vec = constraints.to_vec();

  while !vec.is_empty() {
    let new_substitution = unify_one(&vec[0])?;

    vec = apply_substitution_set(&new_substitution, &vec[1..]);
    sub = sub.merge(&new_substitution);
  }

  Ok(sub)
}

fn unify_one(constraint: &Constraint) -> Result<Substitution, TypeError> {
  let res = match constraint.as_pair() {
    | (Type::Var(a), Type::Var(b)) if b.starts_with("number") && !a.starts_with("number") => {
      unify_var(constraint, a, &constraint.right)?
    },
    | (Type::Var(a), Type::Var(b)) if a.starts_with("number") && !b.starts_with("number") => {
      unify_var(constraint, b, &constraint.left)?
    },
    | (Type::Var(a), other) | (other, Type::Var(a)) => unify_var(constraint, a, other)?,
    | (Type::Unit, Type::Unit) => Substitution::empty(),
    | (Type::Tag(n1, param1), Type::Tag(n2, param2)) if n1 == n2 && param1.len() == param2.len() =>
    {
      let c = param1
        .iter()
        .zip(param2)
        .map(|(a, b)| Constraint::new(constraint.span, a, b))
        .collect::<Vec<_>>();

      unify_constraints(&c)?
    },
    | (Type::Function(arg1, param1), Type::Function(arg2, param2)) => unify_constraints(&[
      Constraint::new(constraint.span, arg1.as_ref(), arg2.as_ref()),
      Constraint::new(constraint.span, param1.as_ref(), param2.as_ref()),
    ])?,
    | (Type::Tuple(param1), Type::Tuple(param2)) if param1.len() == param2.len() => {
      let c = param1
        .iter()
        .zip(param2)
        .map(|(a, b)| Constraint::new(constraint.span, a, b))
        .collect::<Vec<_>>();

      unify_constraints(&c)?
    },
    | _ => {
      return Err(TypeError::TypeMatchingError {
        span: constraint.span,
        expected: constraint.left.clone(),
        found: constraint.right.clone(),
      });
    },
  };

  Ok(res)
}

fn unify_var(constraint: &Constraint, var: &str, ty: &Type) -> Result<Substitution, TypeError> {
  if var == "_" {
    return Ok(Substitution::empty());
  }

  if var.starts_with("number") {
    return match ty {
      | Type::Var(var2) if var == var2 => Ok(Substitution::empty()),
      | Type::Var(var2) if var2.starts_with("number") => Ok(Substitution::var_pair(var, ty)),
      | Type::Tag(name, _) if name == "Int" || name == "Float" => {
        Ok(Substitution::var_pair(var, ty))
      },
      | _ => {
        return Err(TypeError::TypeMatchingError {
          span: constraint.span,
          expected: constraint.left.clone(),
          found: constraint.right.clone(),
        });
      },
    };
  }

  match ty {
    | Type::Var(var2) if var == var2 => Ok(Substitution::empty()),
    | Type::Var(_) => Ok(Substitution::var_pair(var, ty)),
    | _ if occurs(var, ty) => Err(TypeError::RecursiveTypeDefinition {
      span: constraint.span,
      var: var.to_string(),
      ty: ty.clone(),
    }),
    | _ => Ok(Substitution::var_pair(var, ty)),
  }
}

fn occurs(var: &str, ty: &Type) -> bool {
  match ty {
    | Type::Unit => false,
    | Type::Var(var2) => var == var2,
    | Type::Function(a, b) => occurs(var, a) || occurs(var, b),
    | Type::Tag(_, items) | Type::Tuple(items) => items.iter().any(|i| occurs(var, i)),
  }
}

fn apply_substitution_set(sub: &Substitution, cons: &[Constraint]) -> Vec<Constraint> {
  cons
    .iter()
    .map(|c| apply_substitution_constraint(sub, c))
    .collect::<Vec<_>>()
}

fn apply_substitution_constraint(sub: &Substitution, cons: &Constraint) -> Constraint {
  Constraint::new(
    cons.span,
    &apply_substitution_ty(sub, &cons.left),
    &apply_substitution_ty(sub, &cons.right),
  )
}

fn apply_substitution_ty(sub: &Substitution, ty: &Type) -> Type {
  sub.0.iter().fold(ty.clone(), |result, (var, sol_ty)| {
    apply_substitution(&result, var, sol_ty)
  })
}

fn apply_substitution(ty: &Type, var: &Type, replacement: &Type) -> Type {
  match ty {
    | Type::Unit => ty.clone(),
    | Type::Var(_) => {
      if ty == var {
        replacement.clone()
      } else {
        ty.clone()
      }
    },
    | Type::Tag(name, items) => Type::Tag(
      name.clone(),
      items.map(|i| apply_substitution(i, var, replacement)),
    ),
    | Type::Function(a, b) => Type::Function(
      Box::new(apply_substitution(a, var, replacement)),
      Box::new(apply_substitution(b, var, replacement)),
    ),
    | Type::Tuple(items) => Type::Tuple(items.map(|i| apply_substitution(i, var, replacement))),
  }
}

fn replace_pattern_types(sub: &Substitution, annotated: TypedPattern) -> TypedPattern {
  match annotated {
    | TypedPattern::Var(a, b, c) => TypedPattern::Var(a, sub.replace(b), c),
    | TypedPattern::Adt(a, b, c, d) => TypedPattern::Adt(
      a,
      sub.replace(b),
      sub.replace(c),
      d.into_iter()
        .map(|it| replace_pattern_types(sub, it))
        .collect(),
    ),
    | TypedPattern::Wildcard(a) => TypedPattern::Wildcard(a),
    | TypedPattern::Unit(a) => TypedPattern::Unit(a),
    | TypedPattern::Tuple(a, b, c) => TypedPattern::Tuple(
      a,
      sub.replace(b),
      c.into_iter()
        .map(|it| replace_pattern_types(sub, it))
        .collect(),
    ),
    | TypedPattern::List(a, b, c) => TypedPattern::List(
      a,
      sub.replace(b),
      c.into_iter()
        .map(|it| replace_pattern_types(sub, it))
        .collect(),
    ),
    | TypedPattern::BinaryOperator(a, b, c, d, e) => TypedPattern::BinaryOperator(
      a,
      sub.replace(b),
      c,
      Box::new(replace_pattern_types(sub, *d)),
      Box::new(replace_pattern_types(sub, *e)),
    ),
    | TypedPattern::LitInt(a, b) => TypedPattern::LitInt(a, b),
    | TypedPattern::LitString(a, b) => TypedPattern::LitString(a, b),
    | TypedPattern::LitChar(a, b) => TypedPattern::LitChar(a, b),
    | TypedPattern::Alias(a, b, c, d) => TypedPattern::Alias(
      a,
      sub.replace(b),
      Box::new(replace_pattern_types(sub, *c)),
      d,
    ),
  }
}

fn replace_expr_types(sub: &Substitution, annotated: TypedExpression) -> TypedExpression {
  match annotated {
    | TypedExpression::Const(span, ty, a) => TypedExpression::Const(span, sub.replace(ty), a),
    | TypedExpression::Tuple(span, ty, a) => TypedExpression::Tuple(
      span,
      sub.replace(ty),
      a.into_iter().map(|a| replace_expr_types(sub, a)).collect(),
    ),
    | TypedExpression::List(span, ty, a) => TypedExpression::List(
      span,
      sub.replace(ty),
      a.into_iter().map(|a| replace_expr_types(sub, a)).collect(),
    ),
    | TypedExpression::Ref(span, ty, a) => TypedExpression::Ref(span, sub.replace(ty), a),
    | TypedExpression::If(span, ty, a, b, c) => TypedExpression::If(
      span,
      sub.replace(ty),
      Box::new(replace_expr_types(sub, *a)),
      Box::new(replace_expr_types(sub, *b)),
      Box::new(replace_expr_types(sub, *c)),
    ),
    | TypedExpression::Case(span, ty, expr, branches) => TypedExpression::Case(
      span,
      sub.replace(ty),
      Box::new(replace_expr_types(sub, *expr)),
      branches
        .into_iter()
        .map(|(branch_pattern, branch_expr)| (branch_pattern, replace_expr_types(sub, branch_expr)))
        .collect::<Vec<_>>(),
    ),
    | TypedExpression::Lambda(span, ty, a, b) => TypedExpression::Lambda(
      span,
      sub.replace(ty),
      a,
      Box::new(replace_expr_types(sub, *b)),
    ),
    | TypedExpression::Application(span, ty, a, b) => TypedExpression::Application(
      span,
      sub.replace(ty),
      Box::new(replace_expr_types(sub, *a)),
      Box::new(replace_expr_types(sub, *b)),
    ),
    | TypedExpression::Let(span, ty, let_defs, let_expr) => TypedExpression::Let(
      span,
      sub.replace(ty),
      let_defs,
      Box::new(replace_expr_types(sub, *let_expr)),
    ),
  }
}

fn add_pattern_vars_to_env(env: &mut Context, pat: &TypedPattern) {
  match pat {
    | TypedPattern::Var(_, ty, name) => {
      env.set(name, ty.clone());
    },
    | TypedPattern::Adt(_, _, _, items) => {
      items.for_each(|it| add_pattern_vars_to_env(env, it));
    },
    | TypedPattern::Wildcard(_) => {},
    | TypedPattern::Unit(_) => {},
    | TypedPattern::Tuple(_, _, items) => {
      items.for_each(|it| add_pattern_vars_to_env(env, it));
    },
    | TypedPattern::List(_, _, items) => {
      items.for_each(|it| add_pattern_vars_to_env(env, it));
    },
    | TypedPattern::BinaryOperator(_, _, _, a, b) => {
      add_pattern_vars_to_env(env, a);
      add_pattern_vars_to_env(env, b);
    },
    | TypedPattern::LitInt(_, _) => {},
    | TypedPattern::LitString(_, _) => {},
    | TypedPattern::LitChar(_, _) => {},
    | TypedPattern::Alias(_, ty, pat, name) => {
      add_pattern_vars_to_env(env, pat.as_ref());
      env.set(name, ty.clone());
    },
  }
}
