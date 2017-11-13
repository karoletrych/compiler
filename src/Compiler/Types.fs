module Compiler.Types 
open Ast

type Type = 
    {
        AssemblyName : string
        BaseType : Type option
        DeclaredConstructors : Constructor list
        Identifier : TypeIdentifier
        GenericParameters : TypeIdentifier list
        GenericArguments : TypeIdentifier list
        ImplementedInterfaces : Type list
        Methods : Function list
        Fields : Field list 
        NestedTypes : Type list
    }
    member x.BaseTypes = Option.toList x.BaseType @ x.ImplementedInterfaces

and Function = { 
        Name : string    
        Parameters : Parameter list
        ReturnType : TypeIdentifier option
    }
and Constructor = { 
        Parameters : Parameter list 
    }
and Parameter = { Type : TypeIdentifier; ParameterName : string }
and Field = string * TypeIdentifier
