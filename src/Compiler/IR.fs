module Compiler.IR

open Ast

type Module = 
    {
        Identifier : TypeIdentifier
        Functions : Function list
        Classes : Class list
    }

and Class = 
    {
        Identifier : TypeIdentifier
        Fields : Variable list
        Methods : Function list
        BaseClass : TypeIdentifier
        Constructors : Constructor list
    }

and Constructor = {
    Parameters : Variable list
    Body : Instruction list
    LocalVariables : Variable list
}


and Function =
    {
        Name : string
        ReturnType : TypeIdentifier
        Parameters : Variable list
        Body : Instruction list
        LocalVariables : Variable list
        Context : Context
    }

and Context =
| Static
| Instance

and Variable =
    {
        TypeId : TypeIdentifier
        Name : string
    }

and Instruction =
    | Br of int
    | Brfalse of int
    | Brtrue of int

    | CallConstructor of calleeType : TypeIdentifier * argumentTypes : TypeIdentifier list
    | NewObj of calleeType : TypeIdentifier * argumentTypes : TypeIdentifier list
    | CallMethod of 
        calleeType : TypeIdentifier * 
        method : MethodRef * 
        calleeInstructions : Instruction list * 
        argumentsInstructions : Instruction list
    | CallLocalMethod of MethodRef * Instruction list * Instruction list
    | GetExternalField of TypeIdentifier * FieldRef * Instruction list
    | SetExternalField of TypeIdentifier * FieldRef * Instruction list * Instruction list

    | LdThis
    | Ldloc of string
    | Stloc of string
    | Ldarg of string
    | Starg of string
    | Ldfld of string
    | Stfld of string

    | Ceq
    | Cgt
    | Clt
    | Duplicate
    | Label of int
    | LdcI4 of int
    | LdcR4 of single
    | Ldstr of string

    | Add
    | Sub
    | Mul
    | Div
    | Rem
    | Neg
    | Or
    | And

    | Ret of TypeIdentifier option

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
        FieldType : TypeIdentifier
    }