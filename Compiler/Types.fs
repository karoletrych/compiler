module Compiler.Types 

open Compiler.Ast
open System.Reflection

type Assembly =
    {
        FullName : string;
        ExportedTypes : Type list;
    }

and Type = 
    {
      AssemblyName : string
      BaseType : TypeName option
      DeclaredConstructors : Constructor array
      Name : string
      Guid : System.Guid
      GenericParameters : TypeName array
      ImplementedInterfaces : TypeName array
      Methods : Method array
      Fields : Field array }

and TypeName = string

and Method = 
    { 
        MethodName : string    
        Parameters : Parameter array
        ReturnType : TypeName }
and Constructor = 
    { Parameters : Parameter array}
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
