module Compiler.Types 
open Ast

type Type = 
    {
        IsStatic : bool
        BaseType : Type option
        DeclaredConstructors : Constructor list
        Identifier : TypeIdentifier
        GenericParameters : GenericParameterInfo list
        ImplementedInterfaces : Type list
        Methods : Function list
        Fields : Field list 
        NestedTypes : Type list
    }
    member x.BaseTypes = Option.toList x.BaseType @ x.ImplementedInterfaces

and TypeRef =
| ConstructedType of TypeIdentifier
| GenericParameter of GenericParameterInfo

and GenericParameterInfo = GenericTypeDeclarationPlace * int
and GenericTypeDeclarationPlace =
| Class of TypeIdentifier
| Method of TypeIdentifier * string


and Function = { 
    Name : string    
    Parameters : Parameter list
    GenericParameters : GenericParameterInfo list
    ReturnType : TypeRef option
    IsStatic : bool
}

and Constructor = { 
    Parameters : Parameter list 
}

and Parameter = { 
    Type : TypeRef;
    ParameterName : string 
}

and Field = { 
    Type : TypeRef 
    FieldName : string 
    IsStatic : bool 
}
