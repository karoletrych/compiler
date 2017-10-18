#if !(INTERACTIVE)
module Compiler.SemanticChecker
#endif

#if INTERACTIVE
#load "Ast.fs"
#load "Types.fs"
#load "Result.fs"
#endif

open Compiler.Types
open Compiler.Ast
open Compiler.TypeFinder
open Compiler.Result
open System
open System.Reflection

module TypeChecker = 
    let scanAst scanType =
            let get = List.choose id 
            let scanExpression expression = 
                    match expression with
                    | NewExpression(t, args) -> [scanType t]
                    | _ -> []
            let rec scanStatement (statement : Statement) = 
                    match statement with
                    | FunctionCallStatement(FunctionCall(id, types, args))
                        -> types |> List.map scanType
                    | StaticFunctionCallStatement(t, FunctionCall(id,types,args))
                        -> scanType t :: (types |> List.map scanType)
                    | ValueDeclaration(id, typeSpec, expr)
                        -> get [typeSpec] |> List.map scanType
                    | VariableDeclaration(vd)
                        -> match vd with
                           | DeclarationWithType (i,t) -> [scanType t]
                           | FullDeclaration (i,t,e) -> [scanType t]
                           | DeclarationWithInitialization _ -> []
                    | CompoundStatement(cs)
                         -> cs |> List.collect scanStatement
                    | ReturnStatement e -> 
                         match e with 
                         | Some e -> scanExpression e
                         | None -> []
                    | AssignmentStatement (e1,e2) -> scanExpression e1 @ scanExpression e2

            let rec scanDeclaration = function
            | FunctionDeclaration({
                                    Name = identifier;
                                    GenericParameters = genericParameters;
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

               (baseTypes |> List.map (CustomTypeSpec >> scanType))
                    @ (properties |> List.collect (fun (i,t,e) -> [t] |> List.map scanType))
                    @ scanConstructor
            List.map scanDeclaration

    let scanType 
            (typesDictionary : Map<string, Compiler.Types.Type>)
            (typeSpec : TypeSpec) =
            match typeSpec with
            | CustomTypeSpec (ns, CustomType(id, []))  ->
                    if typesDictionary.ContainsKey (typeSpec.ToString()) 
                    then succeedUnit
                    else fail (CannotResolveType typeSpec)
            | CustomTypeSpec (ns, CustomType(id, generics))  ->
                    let noGenericsTypeName = (CustomTypeSpec (ns, (CustomType(id, [])))).ToString()
                    let typeWithNumberOfGenerics = (noGenericsTypeName + "`" + (List.length generics).ToString() ) //TODO: get rid of this shit
                    match typesDictionary
                        |> Map.tryPick (fun key v -> if key.StartsWith(typeWithNumberOfGenerics) then Some v else None ) with
                    | Some v ->
                        if (generics |> List.map ((fun g -> g.ToString()) >> typesDictionary.ContainsKey)) |> List.forall ((=) true)
                        then succeedUnit
                        else fail (CannotResolveType typeSpec)
                    | None -> fail (CannotResolveType typeSpec)
            | _ -> succeedUnit

let scanTypes = TypeChecker.scanAst (TypeChecker.scanType TypeFinder.types)

