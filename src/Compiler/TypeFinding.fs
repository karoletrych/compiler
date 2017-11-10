module Compiler.TypeFinding

open Types
open Ast
open ReferencedAssembliesMetadata
open CompilerResult
open FSharpx.Collections

// let findTypesInModule (knownTypes : Map<TypeIdentifier, Type>) (modul : Module) =
//     let rec createTypeFromClassDeclaration (declaredType : Class) =
//         let getType (declaredType : TypeIdentifier) = 
//             knownTypes
//             |> Map.tryFind declaredType
//             |> Option.defaultWith(
//                 fun() ->
//                 modul.Classes 
//                 |> List.find (fun c -> (Identifier.fromClassDeclaration c) = declaredType) 
//                 |> createTypeFromClassDeclaration)
//         let createFunctionSignature (method : Function) = 
//             {
//                 Parameters = 
//                     method.Parameters 
//                     |> List.map (fun (id, t) -> 
//                     {
//                         Type = fun() -> getType (Identifier.fromTypeSpec t);
//                         ParameterName = id
//                     });
//                 ReturnType = method.ReturnType |> Option.map (fun t -> fun () -> getType (Identifier.fromTypeSpec t))
//                 FunctionName = method.Name
//             }
//         let createConstructor (astCtor : Ast.Constructor) : Types.Constructor = 
//             {
//                 Parameters = astCtor.Parameters 
//                 |> List.map (fun (name,t) -> 
//                     {
//                      ParameterName = name 
//                      Type = fun() -> getType (Identifier.fromTypeSpec t);
//                     });
//             }
//         {
//             AssemblyName = "CHANGEIT"
//             BaseType = (match declaredType.BaseClass with
//                        | Some t -> Identifier.fromTypeSpec t
//                        | None -> BuiltInTypeSpec Object |> Identifier.fromTypeSpec) 
//                        |> getType |> Some;
//             DeclaredConstructors = declaredType.Constructor |> Option.toList |> List.map createConstructor;
//             Fields = declaredType.Properties 
//                      |> List.map (fun prop -> 
//                          (prop.Name, fun () -> prop.Type|> Identifier.fromTypeSpec |>  getType))
//             Identifier = Identifier.fromClassDeclaration declaredType;
//             GenericParameters = [] //TODO: fix
//             GenericArguments = []
//             ImplementedInterfaces = []
//             Methods = declaredType.FunctionDeclarations |> List.map createFunctionSignature;
//             NestedTypes = []
//         }
//     let withNames = List.map (fun c -> (c.Identifier, c)) >> Map.ofList
//     modul.Classes |> List.map createTypeFromClassDeclaration |> withNames

// open System
// let allKnownTypes (modul : Module) =
//     let externalTypes = exportedTypes [Reflection.Assembly.GetAssembly(typeof<obj>)]
//     let allTypes = findTypesInModule externalTypes modul |> Map.union externalTypes
//     Result.succeed (modul, allTypes)

let typeIdentifiersInModule (modul : Module) =
    modul.Classes 
    |> List.map Identifier.fromClassDeclaration

let allKnownTypeIdentifiers (externalTypes: Map<TypeIdentifier, Type>) (modul : Module) =
    let externalTypeIds = 
        externalTypes
        |> Map.toList 
        |> List.map fst
    let allTypes = typeIdentifiersInModule modul |> List.append externalTypeIds
    Result.succeed (modul, allTypes)

