module Compiler.TypeInferenceAst

open Ast

type Expression = {
    Expression : Ast.Expression;
    Type : Types.Type option;
}

let expr e typ = {Expression = e; Type = typ}

type FunctionDeclaration = {
    CompoundStatement : CompoundStatement;
    InferredReturnType : Types.Type;
    InferredParameters : Types.Type list;
}

and Statement = 
  | FunctionCallStatement of FunctionCallExpression
  | CompoundStatement of CompoundStatement
  | IfStatement of Expression * Statement * Statement option
  | WhileStatement of Expression * Statement
  | ReturnStatement of Expression option
  | VariableDeclaration of Ast.Identifier * Ast.TypeSpec option * Expression option
  | ValueDeclaration of Ast.Identifier * Ast.TypeSpec option * Expression
  | BreakStatement


and CompoundStatement = Statement list

and FunctionCallExpression = Ast.Identifier * Ast.Arguments
