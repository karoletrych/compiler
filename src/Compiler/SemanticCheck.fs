module Compiler.SemanticCheck

open CompilerResult
open TypeInference
open Ast
open AstProcessing
open Types

// TODO: Sprawdzanie cyklicznego dziedziczenia
// TODO: sprawdzenie wywolan konstruktorow klas bazowych
// TODO: confilicting module, class, function, variable, field name
// TODO: if(p == 0) return 0; else if(p == 1) return "1"; else if(p == 2) return 2.0;

type LocalVariable = {
    Name : string
    IsReadOnly : bool
    Type : TypeIdentifier
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
let (|Field|_|) types (t : TypeIdentifier) name =
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
let rec private checkAssignment types ownerType acc (assignee, expr) =
    let acc = {acc with Errors = acc.Errors @ (checkExpression types ownerType acc (getExpression expr))}
    match assignee with
    | IdentifierAssignee name -> 
        match name with
        | Variable acc.LocalVariables v -> 
            if v.IsReadOnly then
                {acc with Errors = AssignmentToReadOnlyVariable(name) :: acc.Errors}
            else if v.Type <> getType expr then
                {acc with Errors = InvalidType(name, getType expr, v.Type) :: acc.Errors}
            else acc
        | Field types ownerType f -> 
            match f.IsReadOnly with
            | true -> {acc with Errors = AssignmentToReadOnlyLocalField(name) :: acc.Errors}
            | false ->
                match f.TypeRef with
                | GenericParameter _ -> failwith "not possible in InferLang"
                | ConstructedType t -> 
                    if t <> (getType expr) 
                    then {acc with Errors = InvalidType(name, getType expr, t) :: acc.Errors}
                    else 
                        acc
                | GenericTypeDefinition(_) -> failwith "not possible"
        | _ -> {acc with Errors = UndefinedVariable name :: acc.Errors}
    | MemberFieldAssignee (assignee, name) ->
        let t = getType assignee
        match name with
        | Field types t f -> 
            match f.IsReadOnly with
            | true -> 
                {acc with Errors = AssignmentToReadOnlyFieldOnType(t, name) :: acc.Errors}
            | false ->
                match f.TypeRef with
                | GenericParameter _ -> failwith ""
                | ConstructedType t -> 
                    if t <> (getType expr) 
                    then {acc with Errors = InvalidType(name, getType expr, t) :: acc.Errors}
                    else 
                        {acc with Errors = UndefinedVariable name :: acc.Errors}
        | _ ->
            {acc with Errors = UndefinedVariable name :: acc.Errors}

and private checkExpression (types : Map<TypeIdentifier, Types.Type>) ownerType acc =
    function
    | BinaryExpression(e1, op, e2) -> 
        let t = types.[getType e1]
        match t.Identifier with
        | s when s = Identifier.string ->
            if Option.isNone (t.Methods |> List.tryFind (fun m -> m.Name = "Concat"))
            then [OperatorNotApplicableForGivenTypes(op, getType e1, getType e2)]
            else []
        | i when i = Identifier.int -> []
        | f when f = Identifier.float -> []
        | d when d = Identifier.double -> []
        | _ ->
            if Option.isNone (t.Methods |> List.tryFind (fun m -> m.Name = (operatorMethodName op)))
            then [OperatorNotApplicableForGivenTypes(op, getType e1, getType e2)]
            else []
    | AssignmentExpression(assignee, expr) -> (checkAssignment types ownerType acc (assignee, expr)).Errors
    | LocalFunctionCallExpression(_) -> []
    | IdentifierExpression(_) -> []
    | ListInitializerExpression(_) -> []
    | LiteralExpression(_) -> []
    | InstanceMemberExpression(_, _) -> []
    | NewExpression(_, _) -> [] //TODO: []
    | StaticMemberExpression(_, _) -> failwith "Not Implemented"
    | UnaryExpression(_, _) -> failwith "Not Implemented"

let rec private checkStatements (ownerType, (types : Map<TypeIdentifier, Types.Type>)) body = 
    let idFold acc _  = acc
    let checkExpression = checkExpression types ownerType

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
                    (InvalidType(name, t |> Option.get, getType expr)) :: acc.Errors
                else
                    acc.Errors) 
                @ (checkExpression acc (getExpression expr))
            LocalVariables = {Name = name; IsReadOnly = true; Type = getType expr} :: acc.LocalVariables
        }
    let declarationWithInitialization acc (name, expr) = 
        { acc 
            with 
                LocalVariables = {Name = name; IsReadOnly = false; Type = getType expr} :: acc.LocalVariables
                Errors = acc.Errors @ (checkExpression acc (getExpression expr))
        }
    let declarationWithType acc (name, t) =
        { acc with LocalVariables = {Name = name; IsReadOnly = false; Type = Identifier.typeId t} :: acc.LocalVariables}
    let fullVariableDeclaration acc (name, t, expr) =
        let t = Identifier.typeId t
        {
            Errors = 
                (if (t <> getType expr) 
                then
                    (InvalidType(name, t, getType expr)) :: acc.Errors
                else
                    acc.Errors) @ (checkExpression acc (getExpression expr))
            LocalVariables = {Name = name; IsReadOnly = false; Type = t} :: acc.LocalVariables
        }
    let assignmentStatement (acc : SemanticCheckState) (assignee, expr) =
        (checkAssignment types ownerType acc (assignee, expr))

    statementFold
        idFold 
        idFold 
        valueDeclaration 
        declarationWithInitialization 
        declarationWithType 
        fullVariableDeclaration 
        returnStatement 
        assignmentStatement 
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

let check 
    checkForEntryPoint
    (modules : Ast.Module<InferredTypeExpression> list, types : Map<TypeIdentifier, Types.Type>) =

    if checkForEntryPoint && not (singleEntryPointExists modules)
    then Result.failure (NoEntryPointOrMoreThanOne)
    else
        modules |> List.map (checkModule types) |> Result.merge