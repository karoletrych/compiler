module Compiler.IR

open Ast
open Compiler.TypeInference


type Module = 
    {
        Identifier : TypeIdentifier
        Functions : Method list
        Classes : Class list
    }

and Class = 
    {
        Identifier : TypeIdentifier
        Properties : Parameter list
        Methods : Method list
    }

and MethodRef = 
    {
        MethodName : string
        Parameters : TypeIdentifier list
        IsStatic : bool
    }

and FieldRef = 
    {
        FieldName : string
        IsStatic : bool
    }

and Method =
    {
        Name : string
        ReturnType : TypeIdentifier
        Parameters : Parameter list
        Body : ILInstruction list
    }

and Parameter =
    {
        Type : TypeIdentifier
        Name : string
    }

and ILInstruction =
    | Add
    | Br of int
    | Brfalse of int
    | Brtrue of int
    | CallMethod of TypeIdentifier * MethodRef
    | DeclareLocal of string * TypeIdentifier
    | GetField of TypeIdentifier * FieldRef
    | LoadFromIdentifier of string
    | StoreToIdentifier of Assignee<InferredTypeExpression>
    | Ceq
    | Cge
    | Cgt
    | Cle
    | Clt
    | Dup
    | Div
    | Label of int
    | Ldarg of int16
    | Ldc_I4 of int
    | Ldc_R4 of float
    | Ldc_R8 of double
    | Ldstr of string
    | Ldelem of TypeIdentifier
    | Ldlen
    | Ldloc of int16
    | Ldsfld of Parameter
    | Mul
    | Neg
    | Newarr of TypeIdentifier
    | Pop
    | Rem
    | Ret
    | Starg of int16
    | Stelem of TypeIdentifier
    | Stloc of string
    | Stsfld of Parameter
    | Sub