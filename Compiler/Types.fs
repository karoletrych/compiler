module Compiler.Types 

open Compiler.Ast
open System.Reflection

type Module = 
    {
        Types : Type list;
        Functions : Function list;
    }

and Type = 
    {
      AssemblyName : string
      BaseType : TypeName option
      DeclaredConstructors : Constructor list
      Name : string
      Guid : System.Guid
      GenericParameters : TypeName list
      ImplementedInterfaces : TypeName list
      Methods : Function list
      Fields : Field list }
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

// let createBasicType name : Type = 
//     { Name = name
//       Guid = System.Guid.NewGuid()
//       GenericParameters = []
//       ImplementedInterfaces = []
//       BaseType = None
//       Methods = []
//       Fields = [] }

// let createGenericType name typeParams : Type = 
//     { Name = name
//       Guid = System.Guid.NewGuid()
//       GenericParameters = typeParams
//       ImplementedInterfaces = []
//       BaseType = None
//       Methods = []
//       Fields = [] }
