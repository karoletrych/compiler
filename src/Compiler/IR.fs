module Compiler.IR

open Ast
open TypeInference


type Module = 
    {
        Identifier : TypeIdentifier
        Functions : Method list
        Classes : Class list
    }

and Class = 
    {
        Identifier : TypeIdentifier
        Fields : Variable list
        Methods : Method list
        BaseClass : TypeIdentifier
        Constructors : Constructor list
    }

and Constructor = {
    Parameters : Variable list
    Body : ILInstruction list
    LocalVariables : Variable list
}

and Context =
| Static
| Instance

and MethodRef = 
    {
        MethodName : string
        Parameters : TypeIdentifier list
        Context : Context
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
        Parameters : Variable list
        Body : ILInstruction list
        LocalVariables : Variable list
        Context : Context
    }

and Variable =
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
    | CallLocalMethod of MethodRef
    | GetExternalField of TypeIdentifier * FieldRef
    | Ldloc of string
    | Stloc of string
    | Ldarg of string
    | LdargIdx of int16
    | Starg of string
    | Ldfld of string
    | Stfld of string
    | Ceq
    | Cge
    | Cgt
    | Cle
    | Clt
    | Duplicate
    | Div
    | Label of int
    | LdcI4 of int
    | LdcR4 of single
    | LdcR8 of double
    | Ldstr of string
    | Ldlen
    | Mul
    | Neg
    | CallConstructor of TypeIdentifier * TypeIdentifier list
    | NewObj of TypeIdentifier * TypeIdentifier list
    | Pop
    | Rem
    | Ret
    | RetValue of TypeIdentifier
    | Stsfld of string
    | Sub