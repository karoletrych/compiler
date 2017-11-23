module Compiler.IR

open Ast


type Module = 
    {
        Identifier : TypeIdentifier
        Functions : Method list
        Classes : Class list
    }

and Class = 
    {
        Identifier : TypeIdentifier
        Properties : Variable list
        Methods : Method list
    }

and MethodRef = 
    {
        Name : string
        Parameters : TypeIdentifier list
    }

and Method =
    {
        Name : string
        ReturnType : TypeIdentifier
        Parameters : Variable list
        LocalVariables : Variable list
        Body : ILInstruction list
    }

and Variable =
    {
        Type : TypeIdentifier
        Name : string
    }

and Label = int

and ILInstruction =
    | Add
    | Br of Label
    | Brfalse of Label
    | Brtrue of Label
    | Call of TypeIdentifier * string * TypeIdentifier list
    | Ceq
    | Cge
    | Cgt
    | Cle
    | Clt
    | Dup
    | Div
    | Label of Label
    | Ldarg of int16
    | Ldc_I4 of int
    | Ldc_R4 of float
    | Ldc_R8 of double
    | Ldstr of string
    | Ldelem of TypeIdentifier
    | Ldlen
    | Ldloc of int16
    | Ldsfld of Variable
    | Mul
    | Neg
    | Newarr of TypeIdentifier
    | Pop
    | Rem
    | Ret
    | Starg of int16
    | Stelem of TypeIdentifier
    | Stloc of int16
    | Stsfld of Variable
    | Sub