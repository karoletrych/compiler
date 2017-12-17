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
        else  Result.failure error :: acc

    let idFold acc _  = acc

    let ifStatement acc expr = 
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
        []
    statementFold
        idFold idFold idFold idFold idFold idFold id returnStatement idFold id ifStatement whileStatement idFold
            [] (CompositeStatement body) 

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
let private checkFunction (``function`` : Function<InferredTypeExpression>) = 
    checkStatements ``function``.Body
    |> Result.merge
    |> Result.map (fun statements -> {Name = ``function``.Name; Body = statements; Parameters = ``function``.Parameters; ReturnType = ``function``.ReturnType})

let private checkClass ``class`` =
    Result.succeed ``class``

let private checkModule (modul : Module<InferredTypeExpression>) =
    let functions = modul.Functions 
                    |> List.map checkFunction 
                    |> Result.merge
    let classes = modul.Classes
                  |> List.map checkClass
                  |> Result.merge
    (functions, classes)
    ||> Result.map2 (fun functions classes -> {Identifier = modul.Identifier; Functions = functions; Classes = classes})

let semanticCheck (modules : Ast.Module<InferredTypeExpression> list) =
    modules |> List.map checkModule |> Result.merge
