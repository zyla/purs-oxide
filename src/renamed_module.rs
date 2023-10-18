use std::collections::{HashMap, HashSet};

use salsa::DebugWithDb;

use petgraph::{algo::tarjan_scc, prelude::DiGraph};

use crate::ast::*;
use crate::{symbol::Symbol, Db, ModuleId, ParsedModule};

#[derive(PartialEq, Eq, Clone, Debug, DebugWithDb, Hash)]
pub struct DeclId {
    pub namespace: Namespace,
    pub module: ModuleId,
    pub name: Symbol,
}

#[derive(PartialEq, Eq, Clone, Debug, DebugWithDb, Hash)]
pub enum Namespace {
    Class,
    Type,
    Value,
}

#[derive(PartialEq, Eq, Clone, Debug, DebugWithDb)]
pub struct RenamedModule {
    pub module_id: ModuleId,
    pub imported: Vec<(Option<ModuleId>, DeclId)>,
    pub exported: Vec<DeclId>,
    pub declarations: Vec<Declaration>,
}

#[salsa::tracked]
pub fn renamed_module(db: &dyn Db, module_id: ModuleId) -> RenamedModule {
    let indexed = crate::indexed_module::indexed_module(db, module_id);
    let imported = crate::renamed_module::imported_decls(db, module_id);
    let exported = crate::renamed_module::exported_decls(db, module_id);
    let declarations = vec![];

    let module = crate::parsed_module(db, module_id);

    let mut graph = DiGraph::<Declaration, ()>::new();
    let mut node_indices = HashMap::new();

    for declaration in module.ast.declarations.iter() {
        let node_index = graph.add_node(declaration.clone());
        node_indices.insert(declaration.clone(), node_index);
    }

    // TODO: Add edges between nodes based on data structure
    // for declaration in module.ast.declarations.iter() {
    //     use crate::ast::DeclarationKind;
    //     match &***declaration {
    //         // Handle each variant of DeclarationKind
    //         DeclarationKind::Data { constructors, .. } => {
    //             for constructor in constructors {
    //                 if let Some(target_node) = node_indices
    //                     .get(&DeclarationKind::ValueDeclaration(constructor.name.clone()))
    //                 {
    //                     graph.add_edge(node_indices[declaration], *target_node, ());
    //                 }
    //             }
    //         }
    //         DeclarationKind::TypeSynonym { body, .. } => todo!(),
    //     }
    // }

    let scc = tarjan_scc(&graph);

    RenamedModule {
        module_id,
        imported,
        exported,
        declarations,
    }
}

struct ExportedDeclExtractor<'a> {
    db: &'a dyn Db,
    module_id: ModuleId,
    exported_decls: Vec<DeclId>,
}

impl<'a> ExportedDeclExtractor<'a> {
    fn extract(&mut self, module: &ParsedModule) {
        if let Some(decl_ref_kind) = &module.ast.exports {
            let db = self.db;
            let mut iter = decl_ref_kind.iter().peekable();
            while let Some(ref_decl) = iter.peek().copied() {
                use DeclarationRefKind::*;

                match **ref_decl {
                    TypeClass { name } => {
                        self.exported_decls.push(DeclId {
                            name,
                            module: self.module_id,
                            namespace: Namespace::Class,
                        });
                        iter.next();
                    }
                    TypeOp { name } => {
                        self.exported_decls.push(DeclId {
                            name,
                            module: self.module_id,
                            namespace: Namespace::Type,
                        });
                        iter.next();
                    }
                    Type { name, .. } => {
                        self.exported_decls.push(DeclId {
                            name,
                            module: self.module_id,
                            namespace: Namespace::Type,
                        });
                        iter.next();
                    }
                    Value { name } => {
                        self.exported_decls.push(DeclId {
                            name,
                            module: self.module_id,
                            namespace: Namespace::Value,
                        });
                        iter.next();
                    }
                    ValueOp { name } => {
                        self.exported_decls.push(DeclId {
                            name,
                            module: self.module_id,
                            namespace: Namespace::Value,
                        });
                        iter.next();
                    }
                    TypeInstanceRef { name, .. } => {
                        self.exported_decls.push(DeclId {
                            name,
                            module: self.module_id,
                            namespace: Namespace::Type,
                        });
                        iter.next();
                    }
                    Module { name } => {
                        let module = crate::parsed_module(db, name);
                        let mut inner = ExportedDeclExtractor {
                            db,
                            module_id: name,
                            exported_decls: Default::default(),
                        };
                        inner.extract(&module);
                        self.exported_decls.append(&mut inner.exported_decls);
                        iter.next();
                    }
                }
            }
        }
    }
}

#[salsa::tracked]
pub fn exported_decls(db: &dyn Db, module_id: ModuleId) -> Vec<DeclId> {
    let module = crate::parsed_module(db, module_id);

    let mut extractor = ExportedDeclExtractor {
        db,
        module_id,
        exported_decls: Default::default(),
    };

    extractor.extract(&module);

    extractor.exported_decls
}

pub fn imported_decls(db: &dyn Db, module_id: ModuleId) -> Vec<(Option<ModuleId>, DeclId)> {
    let module = crate::parsed_module(db, module_id);

    let mut imports: Vec<(Option<ModuleId>, DeclId)> = Vec::new();

    let mut iter = module.ast.imports.iter().peekable();
    while let Some(import) = iter.peek().copied() {
        use ImportDeclarationKind::*;
        match &import.kind {
            Implicit => {
                crate::renamed_module::exported_decls(db, import.module)
                    .iter()
                    .for_each(|i| imports.push((import.alias, i.clone())));

                iter.next();
            }
            Explicit(decls) => {
                decls
                    .into_iter()
                    .map(|i| to_decl_id(import.module, &i))
                    .for_each(|i| imports.push((import.alias, i)));

                iter.next();
            }
            Hiding(decls) => {
                let excluded: HashSet<DeclId> = decls
                    .into_iter()
                    .map(|i| to_decl_id(import.module, &i))
                    .collect();
                crate::renamed_module::exported_decls(db, import.module)
                    .iter()
                    .filter(|i| !excluded.contains(i))
                    .for_each(|i| imports.push((import.alias, i.clone())));

                iter.next();
            }
        }
    }

    imports
}

fn to_decl_id(module_id: ModuleId, kind: &DeclarationRefKind) -> DeclId {
    use DeclarationRefKind::*;

    match *kind {
        TypeClass { name } => DeclId {
            name,
            module: module_id,
            namespace: Namespace::Class,
        },
        TypeOp { name } => DeclId {
            name,
            module: module_id,
            namespace: Namespace::Type,
        },
        Type { name, .. } => DeclId {
            name,
            module: module_id,
            namespace: Namespace::Type,
        },
        Value { name } => DeclId {
            name,
            module: module_id,
            namespace: Namespace::Value,
        },
        ValueOp { name } => DeclId {
            name,
            module: module_id,
            namespace: Namespace::Value,
        },
        TypeInstanceRef { name, .. } => DeclId {
            name,
            module: module_id,
            namespace: Namespace::Type,
        },
        Module { .. } => panic!("Cannot map module to DeclId"),
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub enum DesugaredExpr {
    Bind(Box<DesugaredExpr>, Box<DesugaredExpr>),
    Pure(Box<DesugaredExpr>),
    App(Box<DesugaredExpr>, Vec<DesugaredExpr>),
    Unit,
    Literal(Literal<Expr>), // change it to DesugaredExpr
    If {
        cond: Box<DesugaredExpr>,
        then_: Box<DesugaredExpr>,
        else_: Box<DesugaredExpr>,
    },
}

fn desugar_let(declarations: Vec<Declaration>, expression: DesugaredExpr) -> DesugaredExpr {
    declarations
        .into_iter()
        .rev()
        .fold(expression, |acc, declaration| {
            use crate::ast::DeclarationKind;
            let commented_declaration = declaration.1;
            let inner_declaration = commented_declaration.1;
            match inner_declaration {
                DeclarationKind::ValueDeclaration(value_declaration) => {
                    let pattern = value_declaration.ident;
                    let bound_expression = desugar_possibly_guarded_expr(value_declaration.expr);
                    DesugaredExpr::Bind(Box::new(bound_expression), Box::new(acc))
                }
                _ => acc,
            }
        })
}

fn desugar_ado(expr: Expr) -> DesugaredExpr {
    use crate::ast::ExprKind;

    match expr.1 {
        ExprKind::Ado(actions, final_expression) => {
            let mut result = desugar_expr(*final_expression);

            for action in actions.into_iter().rev() {
                use crate::ast::DoItem;
                match action {
                    DoItem::Expr(expr) => {
                        result =
                            DesugaredExpr::Bind(Box::new(desugar_expr(expr)), Box::new(result));
                    }
                    DoItem::Let(declarations) => {
                        result = desugar_let(declarations, result);
                    }
                    DoItem::Bind(pat, expr) => {
                        result = desugar_bind(pat, desugar_expr(expr), result);
                    }
                }
            }

            result
        }
        _ => desugar_expr(expr),
    }
}

fn desugar_possibly_guarded_expr(expr: PossiblyGuardedExpr) -> DesugaredExpr {
    match expr {
        PossiblyGuardedExpr::Unconditional(unconditional_expr) => {
            DesugaredExpr::Pure(Box::new(desugar_expr(unconditional_expr)))
        }
        PossiblyGuardedExpr::Guarded(guarded_exprs) => {
            let mut result = DesugaredExpr::Pure(Box::new(DesugaredExpr::Unit));
            for guarded_expr in guarded_exprs {
                use crate::ast::GuardedExpr;
                match guarded_expr {
                    GuardedExpr { guards, expr } => {
                        for guard in guards {
                            result = DesugaredExpr::Bind(
                                Box::new(desugar_guard(guard, desugar_expr(expr.clone()))),
                                Box::new(result),
                            );
                        }
                        result =
                            DesugaredExpr::Bind(Box::new(desugar_expr(expr)), Box::new(result));
                    }
                }
            }

            result
        }
    }
}

fn desugar_guard(guard: Guard, expr: DesugaredExpr) -> DesugaredExpr {
    match guard {
        Guard::Expr(expr_condition) => DesugaredExpr::If {
            cond: Box::new(desugar_expr(expr_condition)),
            then_: Box::new(expr),
            else_: Box::new(DesugaredExpr::Pure(Box::new(DesugaredExpr::Unit))),
        },
        Guard::Bind(pat, guarded_expr) => {
            DesugaredExpr::Bind(Box::new(desugar_expr(guarded_expr)), Box::new(expr))
        }
    }
}

fn desugar_bind(pattern: Pat, expr: DesugaredExpr, expression: DesugaredExpr) -> DesugaredExpr {
    let inner_pattern = pattern.1;
    match inner_pattern {
        PatKind::Var(symbol) => {
            // Handle a simple variable binding (e.g., x <- expr)
            DesugaredExpr::Bind(Box::new(expr), Box::new(expression))
        }
        // TODO: Handle other needed types of patterns
        _ => expression,
    }
}

fn desugar_expr(expr: Expr) -> DesugaredExpr {
    use crate::ast::ExprKind;
    let inner_expr = expr.1.clone();
    match inner_expr {
        ExprKind::Literal(literal) => {
            DesugaredExpr::Pure(Box::new(DesugaredExpr::Literal(literal)))
        }
        ExprKind::Infix(left, ops) => {
            let desugared_left = desugar_expr(*left);

            let desugared_ops: Vec<DesugaredExpr> = ops
                .into_iter()
                .map(|(infixOp, right)| desugar_infix_op(infixOp, right, desugared_left.clone()))
                .collect();

            let result = desugared_ops
                .into_iter()
                .fold(desugared_left, |acc, desugared_op| {
                    DesugaredExpr::Bind(Box::new(desugared_op), Box::new(acc))
                });

            result
        }
        // TODO: Handle other cases
        _ => DesugaredExpr::from(desugar_expr(expr)),
    }
}

fn desugar_infix_op(op: InfixOp, right_expr: Expr, left_expr: DesugaredExpr) -> DesugaredExpr {
    match op {
        InfixOp::Symbol(s) => {
            // TODO Desugar plus operator
            // Make (a + b) be bind(a, fn(a) => bind(b, fn(b) => pure(a + b)))
            let right_desugared = desugar_expr(right_expr);

            DesugaredExpr::Bind(Box::new(left_expr), Box::new(right_desugared))
        }

        _ => todo!("Handle other operators +, -"),
    }
}
