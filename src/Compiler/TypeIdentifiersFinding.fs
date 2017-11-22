module Compiler.TypeIdentifiersFinding

open Types
open Ast

let typeIdentifiersInModule (modul : Module<AstExpression>) =
    modul.Identifier :: (modul.Classes |> List.map (fun c -> c.Identifier))

let typeIdentifiers (externalTypes: Map<TypeIdentifier, Type>) modules =
    let externalTypeIds = 
        externalTypes
        |> Map.toList 
        |> List.map fst
    modules 
    |> List.collect typeIdentifiersInModule 
    |> List.append externalTypeIds

