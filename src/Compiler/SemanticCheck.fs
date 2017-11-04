#if !(INTERACTIVE)
module Compiler.SemanticCheck
#endif

#if INTERACTIVE
#load "Ast.fs"
#load "Types.fs"
#load "Result.fs"
#endif

open Compiler.Types
open Compiler.Ast
open Compiler.TypeFinding
open Compiler.CompilerResult
open Compiler.Identifier
open System

let transformTypeSpecsInAst scanType =
    let get = List.choose id 
    let scanExpression expression = 
            match expression with
            | NewExpression(t, _) -> [scanType t]
            | _ -> []
    let rec scanStatement (statement : Statement) = 
            match statement with
            | FunctionCallStatement({Name = _; GenericArguments = types; Arguments = _})
                -> types |> List.map scanType
            | StaticFunctionCallStatement(t, {Name = _; GenericArguments = types; Arguments = _})
                -> scanType t :: (types |> List.map scanType)
            | ValueDeclaration(id, typeSpec, expr)
                -> get [typeSpec] |> List.map scanType
            | VariableDeclaration(vd)
                -> match vd with
                   | DeclarationWithType (i,t) -> [scanType t]
                   | FullDeclaration (i,t,e) -> [scanType t]
                   | DeclarationWithInitialization _ -> []
            | CompositeStatement(cs)
                 -> cs |> List.collect scanStatement
            | ReturnStatement e -> 
                 match e with 
                 | Some e -> scanExpression e
                 | None -> []
            | AssignmentStatement (e1,e2) -> scanExpression e1 @ scanExpression e2

    let rec scanDeclaration = function
    | FunctionDeclaration({
                            Parameters = parameters;
                            ReturnType = returnType;
                            Body = cs})
      -> ((get [returnType]) 
         @ (parameters |> List.map snd)
      |> List.map scanType)
         @ (cs |> List.collect scanStatement)
    | ClassDeclaration
        ({
            Name = name;
            GenericTypeParameters = typeParameters;
            ImplementedInterfaces = baseTypes;
            Properties = properties;
            Constructor = constructor;
            FunctionDeclarations = methods})
      ->
       let scanConstructor = 
           match constructor with
           | None -> []
           | Some c ->
               let { Parameters = parameters; BaseClassConstructorCall = baseCall; Statements = stmts} = c
               (parameters |> List.map (snd >> scanType)) @ (stmts |> List.collect scanStatement)

       (baseTypes |> List.map scanType)
            @ (properties |> List.collect (fun (i,t,e) -> [t] |> List.map scanType))
            @ scanConstructor
    List.map scanDeclaration


let checkIfTypeDeclared 
        (typesDictionary : Map<TypeIdentifier, Types.Type>)
        (typeSpec : TypeSpec) =
        match typeSpec with
        | CustomTypeSpec (ns, {Name = id; GenericArgs = []})  ->
                if typesDictionary.ContainsKey (typeSpec |> Identifier.fromTypeSpec) 
                then Result.succeedUnit
                else Result.failure (CannotResolveType typeSpec)
        | CustomTypeSpec (ns, {Name = id; GenericArgs = generics})  ->
                match typesDictionary
                    |> Map.tryPick (fun key v -> if key.TypeName.Name = [id] && key.GenericArgumentsNumber = generics.Length then Some v else None ) with
                | Some v ->
                    if (generics |> List.map ((fun g -> g |> Identifier.fromTypeSpec) >> typesDictionary.ContainsKey)) |> List.forall ((=) true)
                    then Result.succeedUnit
                    else Result.failure (CannotResolveType typeSpec)
                | None -> Result.failure (CannotResolveType typeSpec)
        | _ -> Result.succeedUnit

let resolveTypeSpec (ts : TypeSpec) : CompilerResult<TypeIdentifier> = 
    ts 
    |> Identifier.fromTypeSpec 
    |> Result.succeed

let resolveTypeSpecs (ast : Module)

let scanTypes = transformTypeSpecsInAst (checkIfTypeDeclared mscorlibTypes)

// let scanModule modul =
    // TypeChecker.scanAst (TypeChecker.scanType (allKnownTypes (fst modul) (snd modul)))

