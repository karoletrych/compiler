module Compiler.SemanticCheck

open CompilerResult
open TypeInference
open Ast
open AstProcessing

// TODO: Break -> no enclosing loop
// TODO: Sprawdzanie cyklicznego dziedziczenia
// TODO: if(p == 0) return 0; else if(p == 1) return "1"; else if(p == 2) return 2.0;
// TODO: sprawdzenie wywolan konstruktorow klas bazowych
// TODO: confilicting module, class, function, variable, field name

type LocalVariable = {
    Name : string
    IsReadOnly : bool
}
type SemanticCheckState = {
    LocalVariables : LocalVariable list
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

let private checkExpression (types : Map<TypeIdentifier, Types.Type>) =
    function
    | BinaryExpression(e1, op, e2) -> 
        let t = types.[getType e1]
        match t.Identifier with
        | s when s = Identifier.string ->
            if Option.isNone (t.Methods |> List.tryFind (fun m -> m.Name = "Concat"))
            then [OperatorNotApplicableForGivenTypes(op, getType e1, getType e2)]
            else []
        | _ ->
            if Option.isNone (t.Methods |> List.tryFind (fun m -> m.Name = (operatorMethodName op)))
            then [OperatorNotApplicableForGivenTypes(op, getType e1, getType e2)]
            else []
    | AssignmentExpression(_, _) -> [] //TODO:
    | LocalFunctionCallExpression(_) -> []
    | IdentifierExpression(_) -> []
    | ListInitializerExpression(_) -> []
    | LiteralExpression(_) -> []
    | InstanceMemberExpression(_, _) -> []
    | NewExpression(_, _) -> [] //TODO: []
    | StaticMemberExpression(_, _) -> failwith "Not Implemented"
    | UnaryExpression(_, _) -> failwith "Not Implemented"

let rec private checkStatements (ownerType, (types : Map<TypeIdentifier, Types.Type>)) body = 
    let (|Field|_|) (t : TypeIdentifier) name =
        let t = types |> Map.tryFind t
        match t with
        | Some t ->
            match (t.Fields |> List.tryFind (fun f -> f.FieldName = name)) with
            | Some f -> Some (Field f)
            | None -> None
        | None -> failwith "should fail in type inference"
    let (|Variable|_|) variables name =
        match variables |> List.tryFind (fun v -> v.Name = name) with
        | Some v -> Some (Variable v)
        | None -> None
                
    let idFold acc _  = acc

    let ifExpression acc expr = 
        if getType expr <> Identifier.bool
        then
            {acc with Errors = (NonBooleanExpressionInIfStatement (getType expr)) :: acc.Errors}
        else
            acc
    let whileStatement acc expr = 
        if getType expr <> Identifier.bool
        then
            {acc with Errors = (NonBooleanExpressionInWhileStatement (getType expr)) :: acc.Errors}
        else
            acc
    let returnStatement acc stmt =
        acc
    let ifStatement (s, elseS) = 
        let elseS = elseS |> Option.defaultValue initialState
        s + elseS
    let valueDeclaration acc (name, t, expr) = 
        let t = t |> Option.map Identifier.typeId
        {
            Errors = 
                (if (t |> Option.isSome && t |> Option.get <> getType expr)
                then
                    (InvalidTypeInVariableDeclaration(name, t |> Option.get, getType expr)) :: acc.Errors
                else
                    acc.Errors) @ (checkExpression types (getExpression expr))
            LocalVariables = {Name = name; IsReadOnly = true} :: acc.LocalVariables
        }
    let declarationWithInitialization acc (name, expr) = 
        { acc 
            with 
                LocalVariables = {Name = name; IsReadOnly = false} :: acc.LocalVariables
                Errors = acc.Errors @ (checkExpression types (getExpression expr))
        }
    let declarationWithType acc (name, t) =
        { acc with LocalVariables = {Name = name; IsReadOnly = false} :: acc.LocalVariables}
    let fullVariableDeclaration acc (name, t, expr) =
        let t = Identifier.typeId t
        {
            Errors = 
                (if (t <> getType expr) 
                then
                    (InvalidTypeInVariableDeclaration(name, t, getType expr)) :: acc.Errors
                else
                    acc.Errors) @ (checkExpression types (getExpression expr))
            LocalVariables = {Name = name; IsReadOnly = false} :: acc.LocalVariables
        }
    let assignmentStatement (acc : SemanticCheckState) (assignee, expr) =
        let acc = {acc with Errors = acc.Errors @ (checkExpression types (getExpression expr))}
        match assignee with
        | IdentifierAssignee name -> 
            match name with
            | Variable acc.LocalVariables v -> 
                if v.IsReadOnly then
                    {acc with Errors = AssignmentToReadOnlyVariable(name) :: acc.Errors}
                else acc
            | Field ownerType f -> 
                if f.IsReadOnly then
                    {acc with Errors = AssignmentToReadOnlyLocalField(name) :: acc.Errors}
                else acc
            | _ ->
                {acc with Errors = UndefinedVariable name :: acc.Errors}
        | MemberFieldAssignee (assignee, name) ->
            let t = getType assignee
            match name with
            | Field t f -> 
                if f.IsReadOnly then
                    {acc with Errors = AssignmentToReadOnlyFieldOnType(t, name) :: acc.Errors}
                else acc
            | _ ->
                {acc with Errors = UndefinedVariable name :: acc.Errors}

    statementFold
        idFold 
        idFold 
        valueDeclaration 
        declarationWithInitialization 
        declarationWithType 
        fullVariableDeclaration 
        returnStatement 
        assignmentStatement 
        id 
        ifExpression 
        whileStatement 
        idFold 
        ifStatement
            initialState (CompositeStatement body) 
 
let private checkFunction (ownerType, types) (``function`` : Function<InferredTypeExpression>) : Failure list= 
    (checkStatements (ownerType, types) ``function``.Body).Errors

let private checkClass types ``class`` =
    ``class``.Functions
    |> List.collect (checkFunction (``class``.Identifier, types))

let private checkModule types (modul : Module<InferredTypeExpression>) =
    let functionsErrors = 
        modul.Functions 
        |> List.collect (checkFunction (modul.Identifier, types))
    let classesErrors = 
        modul.Classes
        |> List.collect (checkClass types)
    let errors = (functionsErrors @ classesErrors) |> List.distinct
    match errors with
    | [] -> Result.succeed modul
    | failures -> Failure failures
                  
let singleEntryPointExists (modules : Module<InferredTypeExpression> list)=
    let entryPoints =
            modules
            |> List.collect (fun m -> m.Functions |> List.where (fun f -> f.Name = "main"))
    entryPoints |> List.length = 1

let semanticCheck 
    checkForEntryPoint
    (modules : Ast.Module<InferredTypeExpression> list, types : Map<TypeIdentifier, Types.Type>) =

    if checkForEntryPoint && not (singleEntryPointExists modules)
    then Result.failure (NoEntryPointOrMoreThanOne)
    else
        modules |> List.map (checkModule types) |> Result.merge
