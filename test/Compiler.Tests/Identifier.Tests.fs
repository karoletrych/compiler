module Compiler.Identifier.Tests
open Expecto
open Compiler.Ast

[<Tests>]
let tests =
  testList "Identifier.Tests" [
    testCase "create from class declaration" <| fun _ ->
        let c = {
            Name = "A";
            GenericTypeParameters = [];
            BaseClass = None;
            ImplementedInterfaces = [];
            Properties = [];
            Constructor = None;
            Functions = []}
        let id = Identifier.fromClassDeclaration c

        Expect.equal id.TypeName.Name ["A"] ""
]