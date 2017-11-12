module Compiler.TypeIdentifiersFinding

open Types
open Ast
open ReferencedAssembliesMetadata
open CompilerResult
open FSharpx.Collections

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

