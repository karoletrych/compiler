module Compiler.TypeIdentifiersFinding

open Types
open Ast

let typeIdentifiersInModule (modul : Module) =
    Identifier.fromModule modul :: (modul.Classes |> List.map Identifier.fromClassDeclaration)

let typeIdentifiers (externalTypes: Map<TypeIdentifier, Type>) (modules : Module list) =
    let externalTypeIds = 
        externalTypes
        |> Map.toList 
        |> List.map fst
    modules 
    |> List.collect typeIdentifiersInModule 
    |> List.append externalTypeIds

