#if !(INTERACTIVE)
module Compiler.TypeFinding
#endif

#if INTERACTIVE
#load "Ast.fs"
#load "Types.fs"
#load "Result.fs"
#endif

open FSharpx.Collections
open Compiler.Ast
open Compiler.Types
open Compiler.Identifier
open Compiler.ReferencedAssembliesMetadata
open Compiler.CompilerResult


let findTypesInModule (knownTypes : Type list) (modul : Module) =
    let rec createTypeFromClassDeclaration (declaredType : Class) =
        let getType (declaredType : TypeIdentifier) = 
            knownTypes
            |> List.tryFind (fun t -> t.Identifier = declaredType)
            |> Option.defaultWith (fun () -> 
                modul.Classes 
                |> List.find (fun c -> (Identifier.fromClassDeclaration c) = declaredType) 
                |> createTypeFromClassDeclaration)
        let createFunctionSignature (method : Ast.Function) = 
            {
                Parameters = 
                    method.Parameters 
                    |> List.map (fun (id, t) -> 
                    {
                        Type = fun() -> getType (Identifier.fromTypeSpec t);
                        ParameterName = id
                    });
                ReturnType = method.ReturnType |> Option.map (fun t -> fun () -> getType (Identifier.fromTypeSpec t))
                FunctionName = method.Name
            }
        let createConstructor (astCtor : Ast.Constructor) : Types.Constructor = 
            {
                Parameters = astCtor.Parameters 
                |> List.map (fun (name,t) -> 
                    {
                     ParameterName = name 
                     Type = fun() -> getType (Identifier.fromTypeSpec t);
                    });
            }
        {
            AssemblyName = "CHANGEIT"
            BaseType = (match declaredType.BaseClass with
                       | Some t -> Identifier.fromTypeSpec t
                       | None -> BuiltInTypeSpec Object |> Identifier.fromTypeSpec) 
                       |> getType |> Some;
            DeclaredConstructors = declaredType.Constructor |> Option.toList |> List.map createConstructor;
            Fields = declaredType.Properties 
                     |> List.map (fun property -> 
                         let name, t, _ = property 
                         (name, fun () -> t|> Identifier.fromTypeSpec |>  getType))
            Identifier = Identifier.fromClassDeclaration declaredType;
            GenericParameters = [] //TODO: fix
            GenericArguments = []
            ImplementedInterfaces = []
            Methods = declaredType.FunctionDeclarations |> List.map createFunctionSignature;
            NestedTypes = []
        }
    modul.Classes |> List.map createTypeFromClassDeclaration;

let withNames = List.map (fun c -> (c.Identifier, c))
let userDeclaredTypesWithKnownTypes knownTypes (modul : Module)  =
    findTypesInModule knownTypes modul
    |> withNames
    |> Map.ofList

let userDeclaredTypes (modul : Module) =
    userDeclaredTypesWithKnownTypes [] modul
    

open System
let allKnownTypes (modul : Module) =
    let referencedAssemblies = [Reflection.Assembly.GetAssembly(typeof<obj>)]
    let externalTypes =
        referencedAssemblies 
        |> List.fold (fun state assembly  -> Map.union state (typesFromAssembly assembly)) Map.empty //TODO: if there are 2 types with the same TypeIdentifier second one is chosen

    let userTypes =  userDeclaredTypesWithKnownTypes ((Map.values externalTypes) |> List.ofSeq) modul

    Map.union externalTypes userTypes |> Result.succeed



module TypeSpecSubstitution =
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

    let resolveTypeSpecs (ast : Module) = 
        let substituteTypeSpecs = transformTypeSpecsInAst resolveTypeSpec
        ast.Classes
        |> List.map ClassDeclaration
        |> substituteTypeSpecs

    let scanTypes = transformTypeSpecsInAst (checkIfTypeDeclared mscorlibTypes)