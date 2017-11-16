module Compiler.IRGeneration
open IR
open Ast

let private buildFunctionBody statements : ILInstruction list =
    []


let private buildFunction (builtTypes : Map<TypeIdentifier,  IR.Class>) (func : Ast.Function) = {
    Name = func.Name
    Body = buildFunctionBody func.Body
    ReturnType = 
        builtTypes 
            |> Map.tryFind (Identifier.typeId func.ReturnType.Value) 
            |> Option.defaultValue (buildClass (Identifier.typeId func.ReturnType))
    Parameters = []
    Locals = []
}

let private buildClass (builtTypes : Map<TypeIdentifier,  IR.Class>) (c : Ast.Class) = {
    Methods = c.Functions |> List.map buildFunction
    Properties = c.Properties |> List.map buildProperty
}

let private buildModule (builtTypes : Map<TypeIdentifier,  IR.Class>) (modul : Ast.Module) = {
        Classes = modul.Classes |> List.map buildClass
        Functions = modul.Functions |> List.map buildFunction
    } 
let generateIR modules : IR.Class list =
    modules |> List.collect buildModule
