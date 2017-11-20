module Compiler.TypeIdentifiersFinding

open Types
open Ast

let typeIdentifiersInModule modul =
    Identifier.fromModule modul :: (modul.Classes |> List.map Identifier.fromClassDeclaration)

let typeIdentifiers (externalTypes: Map<TypeIdentifier, Type>) modules =
    let externalTypeIds = 
        externalTypes
        |> Map.toList 
        |> List.map fst
    modules 
    |> List.collect typeIdentifiersInModule 
    |> List.append externalTypeIds

