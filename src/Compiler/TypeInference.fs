module TypeInference

open Compiler.Ast
open Compiler.Parser

type Parameter = 
    { Type : Type }

and Method = 
    { Parameters : Parameter list
      Statements : Statement list }

and Type = 
    { Name : string
      Guid : System.Guid
      TypeParameters : Type list
      ImplementedInterfaces : Type list
      BaseClass : Type option
      Methods : Method list
      Fields : Field list }

and Field = Type * string

let createBasicType name : Type = 
    { Name = name
      Guid = System.Guid.NewGuid()
      TypeParameters = []
      ImplementedInterfaces = []
      BaseClass = None
      Methods = []
      Fields = [] }

let createType name typeParams : Type = 
    { Name = name
      Guid = System.Guid.NewGuid()
      TypeParameters = typeParams
      ImplementedInterfaces = []
      BaseClass = None
      Methods = []
      Fields = [] }

let basicTypes = 
    function 
    | IntLiteral(_) -> createBasicType "int"
    | FloatLiteral(_) -> createBasicType "float"
    | StringLiteral(_) -> createBasicType "string"
    | BoolLiteral(_) -> createBasicType "bool"

type InferredTypeExpression = {
    Expression : Expression;
    Type : Type;
}

type InferredFunctionDeclaration = {
    OriginalFunctionDeclaration : FunctionDeclaration;
    InferredReturnType : Type;
    InferredParameters : Type list;
}

// let inferLocalTypes (func: FunctionDeclaration)= 




// let getExpressions (program : Declaration list ) =
//     let getExprs (statement : Statement) =
//         function 
//         | ExpressionStatement ()
//         | IfStatement ()
//         | ReturnStatement ()
//     function
//     | FunctionDeclaration (id, funParams, typ, statements) ->
//         List.fold (fun stmts s -> stmts @ (getExprs s) ([]:Expression) statements 
let program = parse "
                fun main 
                {
                    val name = 'Karol';
                    val age = 22;
                    val weight = 65.1;
                }"

program;;
