use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use super::fold::{self, ExpressionTreeError};
use super::helpers;
use super::Context;
use crate::ast;
use crate::ast::traverser;
use crate::ast::typed::*;
use crate::ast::untyped::*;
use crate::errors::*;
use crate::loader::{self, *};
use crate::source::{SourceCode, Span};
use crate::utils::path;
use crate::utils::vec::{self, VecExt};

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
      map.extend([(key, apply_substitution_type(sbst, &value))].into_iter())
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

  pub fn typecheck_statement(
    &mut self,
    stmt: &Statement,
  ) -> Result<Vec<TypedStatement>, LangError> {
    let statements = match stmt {
      | Statement::Alias(name, vars, ty) => self.typecheck_alias(name, vars, ty)?,
      | Statement::Adt(name, vars, variants) => self.typecheck_adt(name, vars, variants)?,
      | Statement::Function(definition) => self.typecheck_definition(definition)?,
      | Statement::Port(span, name, ty) => self.typecheck_port(*span, name, ty)?,
      | Statement::Infix(_, _, name, definition_ref) => {
        if let Some(ty) = self.context.get(name) {
          vec![TypedStatement::Infix(
            name.clone(),
            definition_ref.clone(),
            ty.clone(),
          )]
        } else if let Some(ty) = self.context.get(definition_ref) {
          vec![TypedStatement::Infix(
            name.clone(),
            definition_ref.clone(),
            ty.clone(),
          )]
        } else {
          vec![]
        }
      },
    };

    Ok(statements)
  }

  pub fn typecheck_module(
    &mut self,
    modules: &HashMap<String, TypedModule>,
    module: &LoadedModule,
  ) -> Result<TypedModule, LangError> {
    let imports = self.typecheck_module_imports(modules, &module.ast.imports)?;

    // Avoid problems with statement order.
    for statement in &module.ast.statements {
      if let Some(ty) = statement.get_type() {
        self.context.set(statement.get_name().unwrap(), ty.clone());
      }
    }

    // Custom behavior for binary operators.
    for statement in &module.ast.statements {
      if let Statement::Infix(_, _, name, definition_ref) = statement {
        let ty = if let Some(ty) = self.context.get(definition_ref) {
          ty.clone()
        } else {
          // TODO: Is it?
          unreachable!(
            "Infix operator {} where the function {} doesn't have a type header",
            name, definition_ref
          );
        };

        self.context.set(name, ty);
      }
    }

    let declarations = self
      .typecheck_module_statements(&module.ast.statements)
      .map_err(|errors| {
        if errors.len() == 1 {
          errors.into_iter().next().unwrap()
        } else {
          LangError::List(errors)
        }
      })?;

    Ok(TypedModule {
      name: module.src.name.to_string(),
      dependencies: module.dependencies.clone(),
      declarations,
      imports,
    })
  }

  pub fn typecheck_alias(
    &mut self,
    name: &str,
    variables: &[String],
    ty: &Type,
  ) -> Result<Vec<TypedStatement>, LangError> {
    let mut used_vars: HashSet<String> = HashSet::new();

    traverser::traverse_type(&mut used_vars, ty, &|set, node| {
      if let Type::Var(var) = &node {
        set.insert(var.clone());
      }
    });

    if used_vars.len() < variables.len() {
      let unused_vars = variables
        .iter()
        .filter(|it| !used_vars.contains(*it))
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

    if used_vars.len() > variables.len() {
      let unknown_vars = used_vars
        .into_iter()
        .filter(|it| !variables.contains(it))
        .collect::<Vec<String>>();

      return Err(LangError::Typechecker(
        self.source.clone(),
        TypeError::UndeclaredTypeVariables {
          name: name.to_string(),
          values: unknown_vars,
        },
      ));
    }

    let decls: Vec<TypedStatement> = vec![TypedStatement::Alias(TypeAlias {
      name: name.to_string(),
      variables: variables.to_owned(),
      replacement: ty.clone(),
    })];

    Ok(decls)
  }

  pub fn typecheck_adt(
    &mut self,
    name: &str,
    types: &[String],
    variants: &[(Span, String, Vec<Type>)],
  ) -> Result<Vec<TypedStatement>, LangError> {
    let type_variables = types
      .iter()
      .map(|it| Type::Var(it.to_string()))
      .collect::<Vec<_>>();

    let adt_type = Type::Tag(name.to_string(), type_variables);
    let mut declarations = vec![];

    // Any error inside the block should be returned after `exit_block()`. We cannot use
    // `self.context.block()`, because we call `self.check_type()`, it needs an immutable reference
    // to self and `self.context.block()` already has a mutable reference to self.
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
        helpers::build_func_type(&vec::rcons(&new_variant_types, adt_type.clone()))
      } else {
        adt_type.clone()
      };

      declarations.push(TypedStatement::Port(variant.name.clone(), variant_type));
    }

    let adt = Arc::new(Adt {
      name: name.to_owned(),
      types: types.to_owned(),
      variants: adt_variants.map(|(_, variant)| variant.clone()),
    });

    declarations.push(TypedStatement::Adt(name.to_string(), adt));

    Ok(declarations)
  }

  pub fn typecheck_port(
    &mut self,
    span: Span,
    name: &str,
    ty: &Type,
  ) -> Result<Vec<TypedStatement>, LangError> {
    let checked_type = self.check_type(span, ty.clone())?;

    Ok(vec![TypedStatement::Port(name.to_string(), checked_type)])
  }

  pub fn typecheck_definition(
    &mut self,
    function: &Definition,
  ) -> Result<Vec<TypedStatement>, LangError> {
    let typed_function = infer_definition_type(&mut self.context, function)
      .map_err(|it| LangError::Typechecker(self.source.clone(), it))?;

    Ok(vec![TypedStatement::Definition(
      function.name.clone(),
      typed_function,
    )])
  }

  pub fn typecheck_expression(&mut self, expr: &Expression) -> Result<TypedExpression, LangError> {
    infer_expression_type(&mut self.context, expr)
      .map_err(|it| LangError::Typechecker(self.source.clone(), it))
  }

  pub fn check_type(&self, span: Span, ty: Type) -> Result<Type, LangError> {
    self
      .get_canonical_type(span, self.replace_type_alias(ty))
      .map_err(|it| LangError::Typechecker(self.source.clone(), it))
  }

  pub fn get_canonical_type(&self, span: Span, ty: Type) -> Result<Type, TypeError> {
    let next_ty = match ty {
      | Type::Unit => Type::Unit,
      | Type::Var(_) => ty,
      | Type::Tag(name, types) => {
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
          types
            .into_iter()
            .map(|it| self.get_canonical_type(span, it))
            .collect::<Result<Vec<_>, _>>()?,
        )
      },
      | Type::Function(param, rest) => {
        Type::Function(
          Box::new(self.get_canonical_type(span, *param)?),
          Box::new(self.get_canonical_type(span, *rest)?),
        )
      },
      | Type::Tuple(items) => {
        Type::Tuple(
          items
            .into_iter()
            .map(|it| self.get_canonical_type(span, it))
            .collect::<Result<Vec<_>, _>>()?,
        )
      },
    };

    Ok(next_ty)
  }

  pub fn replace_type_alias(&self, ty: Type) -> Type {
    match ty {
      | Type::Tag(name, types) => {
        let params = types
          .into_iter()
          .map(|it| self.replace_type_alias(it))
          .collect::<Vec<_>>();

        if let Some(alias) = self.context.get_type_alias(&name) {
          // TODO: Add proper error handling.
          assert_eq!(params.len(), alias.variables.len());

          let mut map = HashMap::new();

          for (index, param) in params.iter().enumerate() {
            map.insert(Type::Var(alias.variables[index].clone()), param.clone());
          }

          let sbst = Substitution(map);

          let types = params
            .into_iter()
            .map(|it| sbst.replace(it))
            .collect::<Vec<_>>();

          Type::Tag(name, types)
        } else {
          Type::Tag(name, params)
        }
      },
      | Type::Unit => Type::Unit,
      | Type::Var(var) => Type::Var(var),
      | Type::Function(param, rest) => {
        Type::Function(
          Box::new(self.replace_type_alias(*param)),
          Box::new(self.replace_type_alias(*rest)),
        )
      },
      | Type::Tuple(types) => {
        Type::Tuple(
          types
            .into_iter()
            .map(|it| self.replace_type_alias(it))
            .collect(),
        )
      },
    }
  }

  pub fn typecheck_module_imports(
    &mut self,
    modules: &HashMap<String, TypedModule>,
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
      self.typecheck_import(modules, &mut module_imports, import)?;
    }

    Ok(module_imports)
  }

  fn import_qualified(
    &mut self,
    module_name: &str,
    alias: &str,
    module: &TypedModule,
    result: &mut Vec<ImportedModule>,
  ) {
    for declaration in &module.declarations {
      self.import_declaration(declaration, module_name, alias, result);
    }
  }

  fn import_exports(
    &mut self,
    module_name: &str,
    module: &TypedModule,
    exports: &ModuleExports,
    result: &mut Vec<ImportedModule>,
  ) -> Result<(), LangError> {
    let declarations = match exports {
      | ModuleExports::Just(exp) => {
        Self::get_exported_statements(&module.declarations, exp).map_err(Wrappable::wrap)?
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
    declaration: &TypedStatement,
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
      | TypedStatement::Port(name, ty) => {
        result.push(ImportedModule {
          source: module_name.to_string(),
          source_name: name.clone(),
          destination_name: aliased_name.clone(),
        });

        self.add_port(&aliased_name, ty.clone());
      },
      | TypedStatement::Definition(name, def) => {
        result.push(ImportedModule {
          source: module_name.to_string(),
          source_name: name.clone(),
          destination_name: aliased_name.clone(),
        });

        self.add_port(&aliased_name, def.header.clone());
      },
      | TypedStatement::Alias(alias) => {
        self.add_type_alias(alias.clone());
      },
      | TypedStatement::Adt(name, _) => {
        self.add_canonical_type_name(&aliased_name, name);
        self.add_canonical_type_name(name, name);
      },
      | TypedStatement::Infix(name, _, ty) => {
        result.push(ImportedModule {
          source: module_name.to_string(),
          source_name: name.clone(),
          destination_name: name.clone(),
        });

        self.add_port(name, ty.clone());
      },
    }
  }

  fn typecheck_import(
    &mut self,
    modules: &HashMap<String, TypedModule>,
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

  pub fn typecheck_module_statements(
    &mut self,
    statements: &[Statement],
  ) -> Result<Vec<TypedStatement>, Vec<LangError>> {
    let mut statements = statements.iter().collect::<Vec<_>>();

    // Sort by type.
    statements.sort_by_key(|stmt| {
      match *stmt {
        | Statement::Adt(..) => 1,
        | Statement::Alias(..) => 2,
        | Statement::Port(..) => 3,
        | Statement::Infix(..) => 4,
        | Statement::Function(_) => 5,
      }
    });

    let mut declarations = vec![];
    let mut errors = vec![];

    for statement in statements {
      let typed_statements = match self.typecheck_statement(statement) {
        | Ok(statements) => statements,
        | Err(err) => {
          errors.push(err);
          vec![]
        },
      };

      for typed_statement in typed_statements.into_iter() {
        declarations.push(typed_statement.clone());

        match typed_statement {
          | TypedStatement::Definition(name, def) => {
            self.add_port(&name, def.header.clone());
          },
          | TypedStatement::Port(name, ty) => {
            self.add_port(&name, ty.clone());
          },
          | TypedStatement::Alias(alias) => {
            self.add_type_alias(alias.clone());
          },
          | TypedStatement::Adt(name, adt) => {
            self.add_canonical_type_name(&name, &adt.name);
          },
          | TypedStatement::Infix(name, _, ty) => {
            self.add_port(&name, ty.clone());
          },
        }
      }
    }

    // Replace infix definitions with copies of the referenced function.
    for declaration in declarations.clone() {
      if let TypedStatement::Infix(name, infix_def, _) = declaration {
        let mut next_declarations = vec![];

        for declaration in &declarations {
          if let TypedStatement::Definition(definition_name, definition) = declaration {
            if definition_name == &infix_def {
              let mut definition = definition.clone();

              definition.name = name.to_string();
              next_declarations.push(TypedStatement::Definition(name.to_string(), definition));

              break;
            }
          }
        }

        declarations.extend(next_declarations);
      }
    }

    if errors.is_empty() {
      Ok(declarations)
    } else {
      Err(errors)
    }
  }

  fn get_exported_statements(
    typed_statements: &[TypedStatement],
    exports: &[ModuleExport],
  ) -> Result<Vec<TypedStatement>, InterpreterError> {
    let mut exported_statements = Vec::new();

    for export in exports.iter() {
      match export {
        | ModuleExport::Adt(name, adt_exports) => {
          match adt_exports {
            | AdtExports::Just(variants) => {
              for typed_statement in typed_statements.iter() {
                let (variant_name, ty) = match typed_statement {
                  | TypedStatement::Definition(variant_name, definition) => {
                    (variant_name, &definition.header)
                  },
                  | TypedStatement::Port(variant_name, ty) => (variant_name, ty),
                  | _ => continue,
                };

                if variants.contains(variant_name) {
                  if let Type::Tag(tag_name, _) = Self::get_function_return_type(ty) {
                    if &tag_name == name {
                      exported_statements.push(typed_statement.clone());
                    }
                  }
                }
              }
            },
            | AdtExports::All => {
              for typed_statement in typed_statements.iter() {
                let ty = match typed_statement {
                  | TypedStatement::Definition(_, definition) => &definition.header,
                  | TypedStatement::Port(_, ty) => ty,
                  | _ => continue,
                };

                if let Type::Tag(tag_name, _) = Self::get_function_return_type(ty) {
                  if &tag_name == name {
                    exported_statements.push(typed_statement.clone());
                  }
                }
              }
            },
          }

          let next_statement = typed_statements
            .iter()
            .find(|decl| {
              if let TypedStatement::Adt(adt_name, _) = decl {
                adt_name == name
              } else {
                false
              }
            })
            .cloned()
            .ok_or_else(|| {
              InterpreterError::MissingExposing(name.clone(), typed_statements.to_owned())
            })?;

          exported_statements.push(next_statement);
        },
        | ModuleExport::Type(name) => {
          let next_statement = typed_statements
            .iter()
            .find(|typed_statement| {
              if let TypedStatement::Alias(alias) = typed_statement {
                &alias.name == name
              } else if let TypedStatement::Adt(adt_name, _) = typed_statement {
                adt_name == name
              } else {
                false
              }
            })
            .cloned()
            .ok_or_else(|| {
              InterpreterError::MissingExposing(name.clone(), typed_statements.to_owned())
            })?;

          exported_statements.push(next_statement);
        },
        | ModuleExport::BinaryOperator(name) => {
          let next_statement = typed_statements
            .iter()
            .find(|typed_statement| {
              if let TypedStatement::Definition(definition_name, _) = typed_statement {
                definition_name == name
              } else {
                false
              }
            })
            .cloned();

          if let Some(it) = next_statement {
            exported_statements.push(it);
          }
        },
        | ModuleExport::Function(name) => {
          let next_statement = typed_statements
            .iter()
            .find(|typed_statement| {
              if let TypedStatement::Definition(definition_name, _) = typed_statement {
                definition_name == name
              } else {
                false
              }
            })
            .cloned()
            .ok_or_else(|| {
              InterpreterError::MissingExposing(name.clone(), typed_statements.to_owned())
            })?;

          exported_statements.push(next_statement);
        },
      }
    }

    Ok(exported_statements)
  }

  fn get_function_return_type(ty: &Type) -> Type {
    if let Type::Function(_, rest) = ty {
      Self::get_function_return_type(&*rest)
    } else {
      ty.clone()
    }
  }
}

fn unpack_types(ty: &Type) -> Vec<Type> {
  let mut current = ty.clone();
  let mut components = vec![];

  while let Type::Function(param, rest) = current {
    components.push((*param).clone());
    current = (*rest).clone();
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
    let annotated_expr = annotate_expression(env, expr)?;

    // Collect constraints.
    collect_expression_constraints(&mut constraints, &annotated_expr);

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
  let res_expr = replace_expression_types(&substitution, annotated_expr);

  Ok(res_expr)
}

fn infer_definition_type(
  context: &mut Context,
  definition: &Definition,
) -> Result<TypedDefinition, TypeError> {
  let mut constraints = vec![];

  // Exhaustive patterns?
  let (substitution, definition_type, annotated_patterns, annotated_expression) =
    context.block(|ctx| {
      // Type Annotation.
      let definition_type = ctx.next_type();
      let mut annotated_patterns = vec![];

      ctx.set(&definition.name, definition_type.clone());

      for pattern in &definition.patterns {
        annotated_patterns.push(annotate_pattern(ctx, pattern)?);
      }

      for pattern in &annotated_patterns {
        add_pattern_vars_to_context(ctx, pattern);
      }

      let annotated_expr = annotate_expression(ctx, &definition.expression)?;

      // Collect constraints.
      if let Some(ty) = &definition.header {
        let safe_ty = update_type_variables(ctx, &mut HashMap::new(), ty.clone());

        collect_type_definition_constraints(
          &mut constraints,
          &safe_ty,
          &annotated_patterns,
          &annotated_expr,
        );
      };

      for pattern in &annotated_patterns {
        collect_pattern_constraints(&mut constraints, pattern);
      }

      collect_expression_constraints(&mut constraints, &annotated_expr);

      // Function type.
      let mut func_types = annotated_patterns
        .iter()
        .map(|it| it.get_type())
        .collect::<Vec<_>>();

      func_types.push(annotated_expr.get_type());

      constraints.push(Constraint::new(
        annotated_expr.get_span(),
        &definition_type,
        &ast::type_func(func_types),
      ));

      // Constraint solutions.
      let substitution = match unify_constraints(&constraints) {
        | Ok(sbst) => sbst,
        | Err(error) => {
          return Err(error);
        },
      };

      Ok((
        substitution,
        definition_type,
        annotated_patterns,
        annotated_expr,
      ))
    })?;

  // Apply solution.
  let definition_expression = replace_expression_types(&substitution, annotated_expression);
  let definition_patterns = annotated_patterns
    .into_iter()
    .map(|it| replace_pattern_types(&substitution, it))
    .collect::<Vec<_>>();

  let definition_type = substitution.replace(definition_type);

  Ok(TypedDefinition {
    header: definition_type,
    name: definition.name.to_string(),
    patterns: definition_patterns,
    expression: definition_expression,
  })
}

fn update_type_variables(context: &mut Context, map: &mut HashMap<String, Type>, ty: Type) -> Type {
  match ty {
    | Type::Unit => Type::Unit,
    | Type::Var(name) => {
      match map.get(&name).cloned() {
        | Some(var) => var,
        | None => {
          let new_ty = if name.starts_with("comparable") {
            context.next_comparable_type()
          } else if name.starts_with("appendable") {
            context.next_appendable_type()
          } else if name.starts_with("number") {
            context.next_number_type()
          } else {
            context.next_type()
          };

          map.insert(name, new_ty.clone());
          new_ty
        },
      }
    },
    | Type::Function(param, rest) => {
      Type::Function(
        Box::new(update_type_variables(context, map, *param)),
        Box::new(update_type_variables(context, map, *rest)),
      )
    },
    | Type::Tag(name, items) => {
      let vec: Vec<Type> = items
        .into_iter()
        .map(|it| update_type_variables(context, map, it))
        .collect();

      Type::Tag(name, vec)
    },
    | Type::Tuple(items) => {
      let vec: Vec<Type> = items
        .into_iter()
        .map(|it| update_type_variables(context, map, it))
        .collect();

      Type::Tuple(vec)
    },
  }
}

fn annotate_pattern(context: &mut Context, pattern: &Pattern) -> Result<TypedPattern, TypeError> {
  let typed = match pattern {
    | Pattern::Var(span, name) => {
      if context.get(name).is_some() {
        return Err(TypeError::VariableNameShadowed {
          span: pattern.span(),
          name: name.clone(),
        });
      }

      TypedPattern::Var(*span, context.next_type(), name.clone())
    },
    | Pattern::Adt(span, name, items) => {
      let adt_type = match context.get(name) {
        | Some(ty) => ty.clone(),
        | None => {
          return Err(TypeError::MissingDefinition {
            span: *span,
            name: name.to_string(),
          });
        },
      };

      let adt_type = update_type_variables(context, &mut HashMap::new(), adt_type);

      TypedPattern::Adt(
        *span,
        context.next_type(),
        adt_type,
        items
          .iter()
          .map(|it| annotate_pattern(context, it))
          .collect::<Result<_, _>>()?,
      )
    },
    | Pattern::Wildcard(span) => TypedPattern::Wildcard(*span),
    | Pattern::Unit(span) => TypedPattern::Unit(*span),
    | Pattern::Tuple(span, items) => {
      TypedPattern::Tuple(
        *span,
        context.next_type(),
        items
          .iter()
          .map(|it| annotate_pattern(context, it))
          .collect::<Result<_, _>>()?,
      )
    },
    | Pattern::List(span, patterns) => {
      TypedPattern::List(
        *span,
        context.next_type(),
        patterns
          .iter()
          .map(|it| annotate_pattern(context, it))
          .collect::<Result<_, _>>()?,
      )
    },
    | Pattern::BinaryOperator(span, op, a, b) => {
      TypedPattern::BinaryOperator(
        *span,
        context.next_type(),
        op.clone(),
        Box::new(annotate_pattern(context, a)?),
        Box::new(annotate_pattern(context, b)?),
      )
    },
    | Pattern::LitInt(span, value) => TypedPattern::LitInt(*span, *value),
    | Pattern::LitString(span, value) => TypedPattern::LitString(*span, value.clone()),
    | Pattern::LitChar(span, value) => TypedPattern::LitChar(*span, *value),
    | Pattern::Alias(span, pattern, name) => {
      let ty = annotate_pattern(context, pattern)?;
      context.set(name, ty.get_type());

      TypedPattern::Alias(*span, ty.get_type(), Box::new(ty), name.clone())
    },
  };

  Ok(typed)
}

fn annotate_expression(
  context: &mut Context,
  expression: &Expression,
) -> Result<TypedExpression, TypeError> {
  let typed_expression = match expression {
    | Expression::QualifiedRef(span, base, name) => {
      let name = path::qualified_name(base, name);

      let ty = context.get(&name).cloned().ok_or_else(|| {
        TypeError::MissingDefinition {
          span: *span,
          name: name.to_string(),
        }
      })?;

      let ty = update_type_variables(context, &mut HashMap::new(), ty);

      TypedExpression::Ref(*span, ty, name)
    },
    | Expression::Ref(span, name) => {
      let ty = context.get(name).cloned().ok_or_else(|| {
        TypeError::MissingDefinition {
          span: *span,
          name: name.to_string(),
        }
      })?;

      let ty = if let Type::Var(_) = &ty {
        ty
      } else {
        update_type_variables(context, &mut HashMap::new(), ty)
      };

      TypedExpression::Ref(*span, ty, name.clone())
    },
    | Expression::Literal(span, literal) => {
      let value: Value = literal.clone().into();

      if let Value::Number(_) = &value {
        TypedExpression::Const(*span, context.next_number_type(), value)
      } else {
        TypedExpression::Const(*span, value.get_type(), value)
      }
    },
    | Expression::Unit(span) => TypedExpression::Const(*span, context.next_type(), Value::Unit),
    | Expression::Tuple(span, expressions) => {
      let expressions = expressions
        .iter()
        .map(|it| annotate_expression(context, it))
        .collect::<Result<Vec<_>, TypeError>>()?;

      TypedExpression::Tuple(*span, context.next_type(), expressions)
    },
    | Expression::List(span, expressions) => {
      let expressions = expressions
        .iter()
        .map(|it| annotate_expression(context, it))
        .collect::<Result<Vec<_>, TypeError>>()?;

      TypedExpression::List(*span, context.next_type(), expressions)
    },
    | Expression::If(span, cond, then_, else_) => {
      TypedExpression::If(
        *span,
        context.next_type(),
        Box::new(annotate_expression(context, cond)?),
        Box::new(annotate_expression(context, then_)?),
        Box::new(annotate_expression(context, else_)?),
      )
    },
    | Expression::Lambda(span, patterns, expression) => {
      let patterns = patterns
        .iter()
        .map(|it| annotate_pattern(context, it))
        .collect::<Result<Vec<_>, TypeError>>()?;

      let expr = context.block(|ctx| {
        patterns
          .iter()
          .for_each(|it| add_pattern_vars_to_context(ctx, it));

        annotate_expression(ctx, expression)
      })?;

      TypedExpression::Lambda(*span, context.next_type(), patterns, Box::new(expr))
    },
    | Expression::Application(span, a, b) => {
      TypedExpression::Application(
        *span,
        context.next_type(),
        Box::new(annotate_expression(context, a)?),
        Box::new(annotate_expression(context, b)?),
      )
    },
    | Expression::OperatorChain(span, expressions, operators) => {
      match fold::create_expression_tree(expressions, operators) {
        | Ok(tree) => annotate_expression(context, &fold::to_expression(tree))?,
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
      }
    },
    | Expression::Let(span, let_definitions, let_expression) => {
      let (entries, typed_expression) = context.block(|ctx| {
        let mut entries = vec![];

        for let_definition in let_definitions {
          match let_definition {
            | Let::Definition(definition) => {
              let typed_def = infer_definition_type(ctx, definition)?;

              ctx.set(&definition.name, typed_def.header.clone());
              entries.push(TypedLet::Definition(typed_def));
            },
            | Let::Pattern(pattern, expression) => {
              let typed_pattern = annotate_pattern(ctx, pattern)?;
              let typed_expression = annotate_expression(ctx, expression)?;

              add_pattern_vars_to_context(ctx, &typed_pattern);
              entries.push(TypedLet::Pattern(typed_pattern, typed_expression));
            },
          }
        }

        Ok((entries, annotate_expression(ctx, let_expression)?))
      })?;

      TypedExpression::Let(
        *span,
        typed_expression.get_type(),
        entries,
        Box::new(typed_expression),
      )
    },
    | Expression::Match(span, cond, branches) => {
      let annotated_expression = annotate_expression(context, cond)?;
      let mut next_branches = vec![];

      for (branch_pattern, branch_expression) in branches {
        context.block(|ctx| {
          let pattern = annotate_pattern(ctx, branch_pattern)?;

          add_pattern_vars_to_context(ctx, &pattern);
          next_branches.push((pattern, annotate_expression(ctx, branch_expression)?));

          Ok(())
        })?;
      }

      TypedExpression::Match(
        *span,
        context.next_type(),
        Box::new(annotated_expression),
        next_branches,
      )
    },
  };

  Ok(typed_expression)
}

fn collect_type_definition_constraints(
  constraints: &mut Vec<Constraint>,
  ty: &Type,
  patterns: &[TypedPattern],
  expr: &TypedExpression,
) {
  let list = unpack_types(ty);

  if list.len() <= patterns.len() {
    // TODO: Add proper error handling.
    panic!(
      "Too many patterns: {} patterns and {} arguments",
      patterns.len(),
      list.len()
    );
  }

  let mut index = 0;

  while index < patterns.len() {
    constraints.push(Constraint::new(
      patterns[index].get_span(),
      &patterns[index].get_type(),
      &list[index],
    ));

    index += 1;
  }

  let types: Vec<_> = list[index..].to_vec();

  constraints.push(Constraint::new(
    expr.get_span(),
    &ast::type_func(types),
    &expr.get_type(),
  ));
}

fn collect_pattern_constraints(constraints: &mut Vec<Constraint>, typed_pattern: &TypedPattern) {
  match typed_pattern {
    | TypedPattern::Adt(_, ty, ctor_type, patterns) => {
      // TODO: Get rid of `unwrap`.
      let adt_type = unpack_types(ctor_type).into_iter().last().unwrap();
      let mut ctor = vec![];

      for pattern in patterns {
        ctor.push(pattern.get_type());
      }

      ctor.push(adt_type.clone());

      constraints.push(Constraint::new(
        typed_pattern.get_span(),
        ctor_type,
        &ast::type_func(ctor),
      ));

      constraints.push(Constraint::new(typed_pattern.get_span(), ty, &adt_type));
      patterns.for_each(|it| collect_pattern_constraints(constraints, it));
    },
    | TypedPattern::Tuple(_, ty, items) => {
      constraints.push(Constraint::new(
        typed_pattern.get_span(),
        ty,
        &Type::Tuple(items.map(|it| it.get_type())),
      ));

      items.for_each(|it| collect_pattern_constraints(constraints, it));
    },
    | TypedPattern::List(_, ty, patterns) => {
      patterns.for_each(|it| {
        constraints.push(Constraint::new(
          typed_pattern.get_span(),
          ty,
          &ast::type_list(it.get_type()),
        ));

        collect_pattern_constraints(constraints, it);
      });
    },
    | TypedPattern::BinaryOperator(_, ty, op, a, b) => {
      assert_eq!("::", op.as_str());

      constraints.push(Constraint::new(
        typed_pattern.get_span(),
        ty,
        &ast::type_list(a.get_type()),
      ));

      constraints.push(Constraint::new(
        typed_pattern.get_span(),
        &b.get_type(),
        &ast::type_list(a.get_type()),
      ));

      collect_pattern_constraints(constraints, a);
      collect_pattern_constraints(constraints, b);
    },
    | TypedPattern::Alias(_, _, pattern, _) => {
      collect_pattern_constraints(constraints, pattern);
    },
    | TypedPattern::Var(..)
    | TypedPattern::Unit(_)
    | TypedPattern::Wildcard(_)
    | TypedPattern::LitInt(..)
    | TypedPattern::LitString(..)
    | TypedPattern::LitChar(..) => { /* Ignored */ },
  }
}

fn collect_expression_constraints(
  constraints: &mut Vec<Constraint>,
  typed_expression: &TypedExpression,
) {
  match typed_expression {
    | TypedExpression::Tuple(_, ty, expressions) => {
      constraints.push(Constraint::new(
        typed_expression.get_span(),
        ty,
        &Type::Tuple(expressions.map(|it| it.get_type())),
      ));

      for expression in expressions {
        collect_expression_constraints(constraints, expression);
      }
    },
    | TypedExpression::List(_, ty, expressions) => {
      for expression in expressions {
        constraints.push(Constraint::new(
          expression.get_span(),
          ty,
          &ast::type_list(expression.get_type()),
        ));

        collect_expression_constraints(constraints, expression);
      }
    },
    | TypedExpression::If(_, ty, cond, then_, else_) => {
      constraints.push(Constraint::new(
        typed_expression.get_span(),
        &cond.get_type(),
        &ast::type_bool(),
      ));

      constraints.push(Constraint::new(
        typed_expression.get_span(),
        ty,
        &then_.get_type(),
      ));

      constraints.push(Constraint::new(
        typed_expression.get_span(),
        ty,
        &else_.get_type(),
      ));

      collect_expression_constraints(constraints, cond);
      collect_expression_constraints(constraints, then_);
      collect_expression_constraints(constraints, else_);
    },
    | TypedExpression::Match(span, ty, expression, cases) => {
      collect_expression_constraints(constraints, expression);

      for (pattern, expression) in cases {
        collect_pattern_constraints(constraints, pattern);
        collect_expression_constraints(constraints, expression);

        constraints.push(Constraint::new(*span, ty, &expression.get_type()));
      }
    },
    | TypedExpression::Lambda(span, ty, patterns, expression) => {
      let mut chain = vec![];

      for pattern in patterns {
        chain.push(pattern.get_type());
        collect_pattern_constraints(constraints, pattern);
      }

      chain.push(expression.get_type());
      constraints.push(Constraint::new(*span, ty, &ast::type_func(chain)));

      collect_expression_constraints(constraints, expression);
    },
    | TypedExpression::Application(_, ty, a, b) => {
      constraints.push(Constraint::new(
        typed_expression.get_span(),
        &a.get_type(),
        &Type::Function(Box::new(b.get_type()), Box::new(ty.clone())),
      ));

      collect_expression_constraints(constraints, a);
      collect_expression_constraints(constraints, b);
    },
    | TypedExpression::Let(_, _, _, expression) => {
      collect_expression_constraints(constraints, expression);
    },
    | TypedExpression::Ref(..) | TypedExpression::Const(..) => { /* Ignored */ },
  }
}

fn unify_constraints(constraints: &[Constraint]) -> Result<Substitution, TypeError> {
  if constraints.is_empty() {
    return Ok(Substitution::empty());
  }

  let mut sbst = Substitution::empty();
  let mut constraints = constraints.to_vec();

  while !constraints.is_empty() {
    let next_sbst = unify_one(&constraints[0])?;

    constraints = apply_substitution_set(&next_sbst, &constraints[1..]);
    sbst = sbst.merge(&next_sbst);
  }

  Ok(sbst)
}

fn unify_one(constraint: &Constraint) -> Result<Substitution, TypeError> {
  let sbst = match constraint.as_pair() {
    | (Type::Var(a), Type::Var(b)) if b.starts_with("number") && !a.starts_with("number") => {
      unify_var(constraint, a, &constraint.right)?
    },
    | (Type::Var(a), Type::Var(b)) if a.starts_with("number") && !b.starts_with("number") => {
      unify_var(constraint, b, &constraint.left)?
    },
    | (Type::Var(a), other) | (other, Type::Var(a)) => unify_var(constraint, a, other)?,
    | (Type::Unit, Type::Unit) => Substitution::empty(),
    | (Type::Tag(lhs_name, lhs_param), Type::Tag(rhs_name, rhs_param))
      if lhs_name == rhs_name && lhs_param.len() == rhs_param.len() =>
    {
      let constraints = lhs_param
        .iter()
        .zip(rhs_param)
        .map(|(left, right)| Constraint::new(constraint.span, left, right))
        .collect::<Vec<_>>();

      unify_constraints(&constraints)?
    },
    | (Type::Function(lhs_param, lhs_rest), Type::Function(rhs_param, rhs_rest)) => {
      unify_constraints(&[
        Constraint::new(constraint.span, lhs_param.as_ref(), rhs_param.as_ref()),
        Constraint::new(constraint.span, lhs_rest.as_ref(), rhs_rest.as_ref()),
      ])?
    },
    | (Type::Tuple(lhs_param), Type::Tuple(rhs_param)) if lhs_param.len() == rhs_param.len() => {
      let constraints = lhs_param
        .iter()
        .zip(rhs_param)
        .map(|(left, right)| Constraint::new(constraint.span, left, right))
        .collect::<Vec<_>>();

      unify_constraints(&constraints)?
    },
    | _ => {
      return Err(TypeError::TypeMatchingError {
        span: constraint.span,
        expected: constraint.left.clone(),
        found: constraint.right.clone(),
      });
    },
  };

  Ok(sbst)
}

fn unify_var(constraint: &Constraint, var: &str, ty: &Type) -> Result<Substitution, TypeError> {
  if var == "_" {
    return Ok(Substitution::empty());
  }

  if var.starts_with("number") {
    return match ty {
      | Type::Var(other_var) if var == other_var => Ok(Substitution::empty()),
      | Type::Var(other_var) if other_var.starts_with("number") => {
        Ok(Substitution::var_pair(var, ty))
      },
      // TODO: Probably extract `Int` and `Float` to somewhere.
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
    | Type::Var(other_var) if var == other_var => Ok(Substitution::empty()),
    | Type::Var(_) => Ok(Substitution::var_pair(var, ty)),
    | _ if occurs(var, ty) => {
      Err(TypeError::RecursiveTypeDefinition {
        span: constraint.span,
        var: var.to_string(),
        ty: ty.clone(),
      })
    },
    | _ => Ok(Substitution::var_pair(var, ty)),
  }
}

fn occurs(var: &str, ty: &Type) -> bool {
  match ty {
    | Type::Unit => false,
    | Type::Var(other_var) => var == other_var,
    | Type::Function(other_param, other_rest) => {
      occurs(var, other_param) || occurs(var, other_rest)
    },
    | Type::Tag(_, types) | Type::Tuple(types) => types.iter().any(|it| occurs(var, it)),
  }
}

fn apply_substitution_set(sbst: &Substitution, constraint: &[Constraint]) -> Vec<Constraint> {
  constraint
    .iter()
    .map(|it| apply_substitution_constraint(sbst, it))
    .collect::<Vec<_>>()
}

fn apply_substitution_constraint(sbst: &Substitution, constraint: &Constraint) -> Constraint {
  Constraint::new(
    constraint.span,
    &apply_substitution_type(sbst, &constraint.left),
    &apply_substitution_type(sbst, &constraint.right),
  )
}

fn apply_substitution_type(sbst: &Substitution, ty: &Type) -> Type {
  let Substitution(map) = sbst;

  map.iter().fold(ty.clone(), |result, (var, solution)| {
    apply_substitution(&result, var, solution)
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
    | Type::Tag(name, types) => {
      Type::Tag(
        name.clone(),
        types.map(|it| apply_substitution(it, var, replacement)),
      )
    },
    | Type::Function(param, rest) => {
      Type::Function(
        Box::new(apply_substitution(param, var, replacement)),
        Box::new(apply_substitution(rest, var, replacement)),
      )
    },
    | Type::Tuple(types) => Type::Tuple(types.map(|it| apply_substitution(it, var, replacement))),
  }
}

fn replace_pattern_types(sbst: &Substitution, pattern: TypedPattern) -> TypedPattern {
  match pattern {
    | TypedPattern::Var(a, b, c) => TypedPattern::Var(a, sbst.replace(b), c),
    | TypedPattern::Adt(a, b, c, d) => {
      TypedPattern::Adt(
        a,
        sbst.replace(b),
        sbst.replace(c),
        d.into_iter()
          .map(|it| replace_pattern_types(sbst, it))
          .collect(),
      )
    },
    | TypedPattern::Wildcard(a) => TypedPattern::Wildcard(a),
    | TypedPattern::Unit(a) => TypedPattern::Unit(a),
    | TypedPattern::Tuple(a, b, c) => {
      TypedPattern::Tuple(
        a,
        sbst.replace(b),
        c.into_iter()
          .map(|it| replace_pattern_types(sbst, it))
          .collect(),
      )
    },
    | TypedPattern::List(a, b, c) => {
      TypedPattern::List(
        a,
        sbst.replace(b),
        c.into_iter()
          .map(|it| replace_pattern_types(sbst, it))
          .collect(),
      )
    },
    | TypedPattern::BinaryOperator(a, b, c, d, e) => {
      TypedPattern::BinaryOperator(
        a,
        sbst.replace(b),
        c,
        Box::new(replace_pattern_types(sbst, *d)),
        Box::new(replace_pattern_types(sbst, *e)),
      )
    },
    | TypedPattern::LitInt(a, b) => TypedPattern::LitInt(a, b),
    | TypedPattern::LitString(a, b) => TypedPattern::LitString(a, b),
    | TypedPattern::LitChar(a, b) => TypedPattern::LitChar(a, b),
    | TypedPattern::Alias(a, b, c, d) => {
      TypedPattern::Alias(
        a,
        sbst.replace(b),
        Box::new(replace_pattern_types(sbst, *c)),
        d,
      )
    },
  }
}

fn replace_expression_types(
  sbst: &Substitution,
  annotated_expression: TypedExpression,
) -> TypedExpression {
  match annotated_expression {
    | TypedExpression::Const(span, ty, value) => {
      TypedExpression::Const(span, sbst.replace(ty), value)
    },
    | TypedExpression::Tuple(span, ty, expressions) => {
      TypedExpression::Tuple(
        span,
        sbst.replace(ty),
        expressions
          .into_iter()
          .map(|it| replace_expression_types(sbst, it))
          .collect(),
      )
    },
    | TypedExpression::List(span, ty, expressions) => {
      TypedExpression::List(
        span,
        sbst.replace(ty),
        expressions
          .into_iter()
          .map(|it| replace_expression_types(sbst, it))
          .collect(),
      )
    },
    | TypedExpression::Ref(span, ty, a) => TypedExpression::Ref(span, sbst.replace(ty), a),
    | TypedExpression::If(span, ty, cond, then_, else_) => {
      TypedExpression::If(
        span,
        sbst.replace(ty),
        Box::new(replace_expression_types(sbst, *cond)),
        Box::new(replace_expression_types(sbst, *then_)),
        Box::new(replace_expression_types(sbst, *else_)),
      )
    },
    | TypedExpression::Match(span, ty, expression, branches) => {
      TypedExpression::Match(
        span,
        sbst.replace(ty),
        Box::new(replace_expression_types(sbst, *expression)),
        branches
          .into_iter()
          .map(|(branch_pattern, branch_expr)| {
            (branch_pattern, replace_expression_types(sbst, branch_expr))
          })
          .collect::<Vec<_>>(),
      )
    },
    | TypedExpression::Lambda(span, ty, a, b) => {
      TypedExpression::Lambda(
        span,
        sbst.replace(ty),
        a,
        Box::new(replace_expression_types(sbst, *b)),
      )
    },
    | TypedExpression::Application(span, ty, a, b) => {
      TypedExpression::Application(
        span,
        sbst.replace(ty),
        Box::new(replace_expression_types(sbst, *a)),
        Box::new(replace_expression_types(sbst, *b)),
      )
    },
    | TypedExpression::Let(span, ty, let_defs, let_expr) => {
      TypedExpression::Let(
        span,
        sbst.replace(ty),
        let_defs,
        Box::new(replace_expression_types(sbst, *let_expr)),
      )
    },
  }
}

fn add_pattern_vars_to_context(context: &mut Context, pattern: &TypedPattern) {
  match pattern {
    | TypedPattern::Var(_, ty, name) => {
      context.set(name, ty.clone());
    },
    | TypedPattern::Adt(_, _, _, patterns) => {
      patterns.for_each(|it| add_pattern_vars_to_context(context, it));
    },
    | TypedPattern::Tuple(_, _, patterns) => {
      patterns.for_each(|it| add_pattern_vars_to_context(context, it));
    },
    | TypedPattern::List(_, _, patterns) => {
      patterns.for_each(|it| add_pattern_vars_to_context(context, it));
    },
    | TypedPattern::BinaryOperator(_, _, _, lhs_pattern, rhs_pattern) => {
      add_pattern_vars_to_context(context, lhs_pattern);
      add_pattern_vars_to_context(context, rhs_pattern);
    },
    | TypedPattern::Alias(_, ty, pattern, name) => {
      add_pattern_vars_to_context(context, pattern.as_ref());
      context.set(name, ty.clone());
    },
    | TypedPattern::Unit(_)
    | TypedPattern::Wildcard(_)
    | TypedPattern::LitInt(..)
    | TypedPattern::LitString(..)
    | TypedPattern::LitChar(..) => { /* Ignored */ },
  }
}
