module TypeInference
open Compiler.Ast
open Compiler.Parser

type Type =
        {
            Name : string;
            Guid : System.Guid;
            TypeParameters : Type list;
            ImplementedInterfaces : Type list;
            BaseClass : Type option
        }

let createType name typeParams : Type = 
                                        {
                                            Name = name;
                                            Guid = System.Guid.NewGuid();
                                            TypeParameters = typeParams;
                                            ImplementedInterfaces = [];
                                            BaseClass = None
                                        }

type ExpressionRef =
    Expression * int

type Constraint =
     ExpressionRef * Type 

type ConstraintTable =
    Expression * Constraint list
// let getExpressions (program : Declaration list ) =
//     let getExprs (statement : Statement) =
//         function 
//         | ExpressionStatement ()
//         | IfStatement ()
//         | ReturnStatement ()
//     function
//     | FunctionDeclaration (id, funParams, typ, statements) ->
//         List.fold (fun stmts s -> stmts @ (getExprs s) ([]:Expression) statements 
     


// getExpressions program;;

