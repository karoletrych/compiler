module Compiler.SemanticCheck

open CompilerResult
open TypeInference
open Ast
open AstProcessing

// Value declaration i variableDeclaration.FullDeclaration type matches expressionType
// Declarations: variable was not defined
// Operator jest aplikowalny dla typu
// Value is not being assigned after initialization
// Variable already defined
// Identifier not defined
// Invalid argument
// Wrong number of arguments
// Break -> no enclosing loop
// Function already defined.
// No entry point (main)
// Sprawdzanie cyklicznego dziedziczenia
// Nazwy zmiennych w ramach funkcji (nie scopea) są unikalne
// argumenty dla operatorów są poprawne
// if(p == 0) return 0; else if(p == 1) return "1"; else if(p == 2) return 2.0;
// sprawdzenie wywolan konstruktorow klas bazowych
// confilicting module, class, function, variable, field name

type SemanticCheckState = {
    LocalVariables : string list
    Errors : Failure list
}
with static member (+) (s1,s2) = {
        LocalVariables = s1.LocalVariables @ s2.LocalVariables
        Errors = s1.Errors @ s2.Errors
}

let initialState = {
    LocalVariables = []
    Errors = []
}

let private checkExpression =
    function
    | BinaryExpression(e1,op,e2) -> Result.succeed (BinaryExpression(e1,op,e2))
    | AssignmentExpression(_, _) -> failwith "Not Implemented"
    | LocalFunctionCallExpression(_) -> failwith "Not Implemented"
    | IdentifierExpression(_) -> failwith "Not Implemented"
    | ListInitializerExpression(_) -> failwith "Not Implemented"
    | LiteralExpression(_) -> failwith "Not Implemented"
    | InstanceMemberExpression(_, _) -> failwith "Not Implemented"
    | NewExpression(_, _) -> failwith "Not Implemented"
    | StaticMemberExpression(_, _) -> failwith "Not Implemented"
    | UnaryExpression(_, _) -> failwith "Not Implemented"

let rec private checkStatements body = 
    let ifNotTrueAddFailure predicate error acc = 
        if predicate
        then acc
        else {acc with Errors = error :: acc.Errors }

    let idFold acc _  = acc

    let ifExpression acc expr = 
        acc 
        |> ifNotTrueAddFailure 
            (getType expr = Identifier.bool) 
            (NonBooleanExpressionInIfStatement (getType expr))
    let whileStatement acc expr = 
        acc
        |> ifNotTrueAddFailure 
            (getType expr = Identifier.bool) 
            (NonBooleanExpressionInWhileStatement (getType expr))
    let returnStatement acc stmt =
        acc
    let ifStatement (s, elseS) = 
        let elseS = elseS |> Option.defaultValue initialState
        s + elseS
    statementFold
        idFold idFold idFold idFold idFold idFold returnStatement idFold id ifExpression whileStatement idFold ifStatement
            initialState (CompositeStatement body) 

    // function
    // | AssignmentStatement(_, _) -> failwith "Not Implemented"
    // | BreakStatement -> failwith "Not Implemented"
    // | CompositeStatement(_) -> failwith "Not Implemented"
    // | LocalFunctionCallStatement(_) -> failwith "Not Implemented"
    // | InstanceMemberFunctionCallStatement(_, _) -> failwith "Not Implemented"
    // | ReturnStatement(_) -> failwith "Not Implemented"
    // | StaticFunctionCallStatement(_, _) -> failwith "Not Implemented"
    // | VariableDeclaration(_) -> failwith "Not Implemented"
    // | ValueDeclaration(_) -> failwith "Not Implemented"
let private checkFunction (``function`` : Function<InferredTypeExpression>) : Failure list= 
    (checkStatements ``function``.Body).Errors

let private checkClass ``class`` =
    []

let private checkModule (modul : Module<InferredTypeExpression>) =
    let functionsErrors = modul.Functions 
                          |> List.collect checkFunction 
    let classesErrors = modul.Classes
                        |> List.collect checkClass
    let errors = (functionsErrors @ classesErrors) |> List.distinct
    match errors with
    | [] -> Result.succeed modul
    | failures -> Failure failures
                  
    

let semanticCheck (modules : Ast.Module<InferredTypeExpression> list) =
    modules |> List.map checkModule |> Result.merge
