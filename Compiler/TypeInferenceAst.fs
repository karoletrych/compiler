module Compiler.TypeInferenceAst

open Ast

type Expression = {
    Expression : Ast.Expression;
    Type : Types.Type option;
}


type FunctionDeclaration = {
    Parameters : Types.Type list;
    CompoundStatement : CompoundStatement;
    InferredReturnType : Types.Type;
}

and Statement = 
  | FunctionCallStatement of FunctionCall
  | CompoundStatement of CompoundStatement
  | IfStatement of Expression * Statement * Statement option
  | WhileStatement of Expression * Statement
  | ReturnStatement of Expression option
  | VariableDeclaration of Ast.Identifier * Ast.TypeSpec option * Expression option
  | ValueDeclaration of Ast.Identifier * Ast.TypeSpec option * Expression
  | BreakStatement


and CompoundStatement = Statement list

and FunctionCall = Ast.Identifier * Ast.Arguments
