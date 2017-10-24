module Compiler.Types 

open Compiler.Ast
open System.Reflection

type Type = 
    {
      AssemblyName : string
      BaseType : TypeName option
      DeclaredConstructors : Constructor list
      Name : string
      GenericParameters : TypeName list
      ImplementedInterfaces : TypeName list
      Methods : Function list
      Fields : Field list 
      NestedTypes : TypeName list
      }
      member x.BaseTypes = Option.toList x.BaseType @ x.ImplementedInterfaces



and TypeName = string

and Function = 
    { 
        FunctionName : string    
        Parameters : Parameter list
        ReturnType : TypeName option}
and Constructor = 
    { Parameters : Parameter list}
and Parameter = { Type : TypeName; ParameterName : string}
and Field = string * TypeName

module Function = 
    let createFunctionDeclaration (method : Ast.Function) = 
        {
            Parameters = method.Parameters |> List.map (fun (id, t) -> {Type = t.ToString(); ParameterName = id});
            ReturnType = method.ReturnType |> Option.map (fun t -> t.ToString())
            FunctionName = method.Name
        }
