module Compiler.Types 

open Compiler.Ast


[<CustomEquality>]
[<CustomComparison>]
type Type = 
    {
        AssemblyName : string
        BaseType : Type option
        DeclaredConstructors : Constructor list
        Identifier : string
        GenericParameters : TypeRef list
        GenericArguments : TypeRef list
        ImplementedInterfaces : Type list
        Methods : Function list
        Fields : Field list 
        NestedTypes : Type list
    }
    member x.BaseTypes = Option.toList x.BaseType @ x.ImplementedInterfaces
    override x.GetHashCode() =
        hash (x.Identifier)
    override x.Equals(y) =
        match y with
        | :? Type as t -> t.Identifier = x.Identifier
        | _ -> false
    interface System.IComparable with
        member x.CompareTo(y) =
            match y with
            | :? Type as t -> System.String.Compare(t.Identifier, x.Identifier)
            | _ -> 0

and TypeRef = unit -> Type

and Function = 
    { 
        FunctionName : string    
        Parameters : Parameter list
        ReturnType : TypeRef option
    }
and Constructor = 
    { Parameters : Parameter list }
and Parameter = { Type : TypeRef; ParameterName : string }
and Field = string * TypeRef
