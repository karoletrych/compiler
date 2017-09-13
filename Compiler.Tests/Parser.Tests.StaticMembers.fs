module Compiler.Parser.Tests.StaticMembers

open Compiler.Ast
open Compiler.Parser
open Expecto

[<Tests>]
let tests = 
    testList "Parser.Tests.StaticMembers" 
        [ 
            testCase "static function call statement" <| fun _ -> 
              let source = " fun main
                        {
                            System::Console:.WriteLine();
                        }"
              Expect.equal (Compiler.Parser.parse source) [FunctionDeclaration
                   (Identifier "main", [], [], None,
                    [StaticFunctionCallStatement
                       (CustomType
                          ([Identifier "System"],
                           NonGenericCustomTypeSpec (NonGenericTypeSpec (Identifier "Console"))),
                        FunctionCall (Identifier "WriteLine",[],[]))])] ""
            testCase "static function call expression" <| fun _ -> 
              let source = " fun main
                        {
                            var s = System::Console:.ReadLine();
                        }"
              Expect.equal (Compiler.Parser.parse source) [FunctionDeclaration
               (Identifier "main", [], [], None,
                [VariableDeclaration
                   (DeclarationWithInitialization
                      (Identifier "s",
                       StaticMemberExpression
                         (CustomType
                            ([Identifier "System"],
                             NonGenericCustomTypeSpec
                               (NonGenericTypeSpec (Identifier "Console"))),
                          FunctionCall (Identifier "ReadLine",[],[]))))])] ""
            testCase "fully qualified type" <| fun _ -> 
              let source = " fun main
                        {
                            var s : System::Object;
                        }"
              Expect.equal (Compiler.Parser.parse source) 
                                [FunctionDeclaration
                                   (Identifier "main", [], [], None,
                                    [VariableDeclaration
                                       (DeclarationWithType
                                          (Identifier "s",
                                           CustomType
                                             ([Identifier "System"],
                                              NonGenericCustomTypeSpec
                                                (NonGenericTypeSpec (Identifier "Object")))))])] ""
            testCase "fully qualified type" <| fun _ -> 
              let source = " fun main (o : System::Object)
                        {
                            System::Console:.WriteLine(o);
                        }"
              Expect.equal (Compiler.Parser.parse source) [FunctionDeclaration
               (Identifier "main", [],
                [(Identifier "o",
                  CustomType
                    ([Identifier "System"],
                     NonGenericCustomTypeSpec (NonGenericTypeSpec (Identifier "Object"))))],
                None,
                [StaticFunctionCallStatement
                   (CustomType
                      ([Identifier "System"],
                       NonGenericCustomTypeSpec (NonGenericTypeSpec (Identifier "Console"))),
                    FunctionCall
                      (Identifier "WriteLine",[],[IdentifierExpression (Identifier "o")]))])]""
        ]
