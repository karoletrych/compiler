module Compiler.Types 

open Compiler.Ast

type Parameter = 
    { Type : Type }

and Function = 
    { Parameters : Parameter list
      Statements : Ast.Statement list }

and Type = 
    { Name : string
      Guid : System.Guid
      TypeParameters : Type list
      ImplementedInterfaces : Type list
      BaseClass : Type option
      Methods : Function list
      Fields : Field list }

and Field = Type * string

let createBasicType name : Type = 
    { Name = name
      Guid = System.Guid.NewGuid()
      TypeParameters = []
      ImplementedInterfaces = []
      BaseClass = None
      Methods = []
      Fields = [] }

let createGenericType name typeParams : Type = 
    { Name = name
      Guid = System.Guid.NewGuid()
      TypeParameters = typeParams
      ImplementedInterfaces = []
      BaseClass = None
      Methods = []
      Fields = [] }
