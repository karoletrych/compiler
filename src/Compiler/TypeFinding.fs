module Compiler.TypeFinding
open Compiler.Types
open Compiler.Ast
open Compiler.CompilerResult
open FSharpx.Collections

let findTypesInModule (knownTypes : Map<TypeIdentifier, Type>) (modul : Module) =
    let rec createTypeFromClassDeclaration (declaredType : Class) =
        let getType (declaredType : TypeSpec) = 
            let (TypeIdentifier typeId) = declaredType
            knownTypes
            |> Map.tryFind typeId
            |> Option.defaultWith(
                fun() ->
                    modul.Classes 
                    |> List.find (fun c -> (Identifier.fromClassDeclaration c) = typeId) 
                    |> createTypeFromClassDeclaration)
        let createFunctionSignature (method : Function) = 
            {
                Parameters = 
                    method.Parameters 
                    |> List.map (fun (id, t) -> 
                    {
                        Type = fun() -> getType (t);
                        ParameterName = id
                    });
                ReturnType = method.ReturnType |> Option.map (fun t -> fun () -> getType (t))
                FunctionName = method.Name
            }
        let createConstructor (astCtor : Ast.Constructor) : Types.Constructor = 
            {
                Parameters = astCtor.Parameters 
                |> List.map (fun (name,t) -> 
                    {
                     ParameterName = name 
                     Type = fun() -> getType (t);
                    });
            }
        {
            AssemblyName = "CHANGEIT"
            BaseType = (match declaredType.BaseClass with
                       | Some t -> t
                       | None -> TypeIdentifier Identifier.object)
                       |> getType |> Some;
            DeclaredConstructors = declaredType.Constructor |> Option.toList |> List.map createConstructor;
            Fields = declaredType.Properties 
                     |> List.map (fun prop -> 
                         (prop.Name, fun () -> prop.Type|> getType))
            Identifier = Identifier.fromClassDeclaration declaredType;
            GenericParameters = [] //TODO: fix
            GenericArguments = []
            ImplementedInterfaces = []
            Methods = declaredType.FunctionDeclarations |> List.map createFunctionSignature;
            NestedTypes = []
        }
    let withNames = List.map (fun c -> (c.Identifier, c)) >> Map.ofList
    modul.Classes |> List.map createTypeFromClassDeclaration |> withNames

let allKnownTypes externalTypes (modul : Module) =
    let allTypes = findTypesInModule externalTypes modul |> Map.union externalTypes
    Result.succeed (modul, allTypes)
