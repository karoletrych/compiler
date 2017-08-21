module Compiler.TypeResolving

open System
open System.Reflection

open Compiler.Types


let getBuiltInTypes =
        function 
        | Ast.IntLiteral(_) -> createBasicType "int"
        | Ast.FloatLiteral(_) -> createBasicType "float"
        | Ast.StringLiteral(_) -> createBasicType "string"
        | Ast.BoolLiteral(_) -> createBasicType "bool"

