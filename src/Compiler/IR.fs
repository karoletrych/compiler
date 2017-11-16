module Compiler.IR

open System

type Module = 
    {
        Functions : Method list;
        Classes : Class list;
    }

and Class = 
    {
        Properties  : Variable list;
        Methods : Method list;
    }

and Method =
    {
        Name       : string;
        ReturnType : Type;
        Parameters : Variable list;
        Locals     : Variable list;
        Body       : ILInstruction list;
    }

and Variable =
    {
        Type  : Type;
        Name  : string;
    }

and Label = int

and ILInstruction =
    | Add
    | Br of Label
    | Brfalse of Label
    | Brtrue of Label
    | Call of string
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
    | Ldc_R8 of float
    | Ldelem of Type
    | Ldlen
    | Ldloc of int16
    | Ldsfld of Variable
    | Mul
    | Neg
    | Newarr of Type
    | Pop
    | Rem
    | Ret
    | Starg of int16
    | Stelem of Type
    | Stloc of int16
    | Stsfld of Variable
    | Sub

    override x.ToString() =
        match x with
        | Add         -> "add"
        | Br(l)       -> sprintf "br %i" l
        | Brfalse(l)  -> sprintf "brfalse %i" l
        | Brtrue(l)   -> sprintf "brtrue %i" l
        | Call(s)     -> sprintf "call %s" s
        | Ceq         -> "ceq"
        | Cge         -> "cge"
        | Cgt         -> "cgt"
        | Cle         -> "cle"
        | Clt         -> "clt"
        | Dup         -> "dup"
        | Div         -> "div"
        | Label(l)    -> sprintf "label %i" l
        | Ldarg(s)    -> sprintf "ldarg %i" s
        | Ldc_I4(i)   -> sprintf "ldc_i4 %i" i
        | Ldc_R8(f)   -> sprintf "ldc_r8 %f" f
        | Ldelem(t)   -> sprintf "ldelem %s" t.Name
        | Ldlen       -> "ldlen"
        | Ldloc(s)    -> sprintf "ldloc %i" s
        | Ldsfld(v)   -> sprintf "ldsfld %s" v.Name
        | Mul         -> "mul"
        | Neg         -> "neg"
        | Newarr(t)   -> sprintf "newarr %s" t.Name
        | Pop         -> "pop"
        | Rem         -> "rem"
        | Ret         -> "ret"
        | Starg(s)    -> sprintf "starg %i" s
        | Stelem(t)   -> sprintf "stelem %s" t.Name
        | Stloc(s)    -> sprintf "stloc %i" s
        | Stsfld(v)   -> sprintf "stsfld %s" v.Name
        | Sub -> "sub"