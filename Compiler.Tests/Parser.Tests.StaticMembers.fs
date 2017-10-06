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
                   { Name = Identifier "main";GenericParameters = []; Parameters =  []; ReturnType = None;
                    Body = [StaticFunctionCallStatement
                       (CustomTypeSpec
                          ([Identifier "System"],
                           CustomType((Identifier "Console"),[])),
                        FunctionCall (Identifier "WriteLine",[],[]))]}] ""
            testCase "static function call expression" <| fun _ -> 
              let source = " fun main
                        {
                            var s = System::Console:.ReadLine();
                        }"
              Expect.equal (Compiler.Parser.parse source) [FunctionDeclaration
               {Name = Identifier "main";GenericParameters = []; Parameters = []; ReturnType =  None;
                Body = [VariableDeclaration
                   (DeclarationWithInitialization
                      (Identifier "s",
                       StaticMemberExpression
                         (CustomTypeSpec
                            ([Identifier "System"],
                             CustomType
                               ((Identifier "Console"),[])),
                          FunctionCall (Identifier "ReadLine",[],[]))))]}] ""
            testCase "fully qualified type" <| fun _ -> 
              let source = " fun main
                        {
                            var s : System::Object;
                        }"
              Expect.equal (Compiler.Parser.parse source) 
                                [FunctionDeclaration
                                   {
                                    Name = Identifier "main"; GenericParameters = []; Parameters = []; ReturnType = None;
                                    Body = [VariableDeclaration
                                       (DeclarationWithType
                                          (Identifier "s",
                                           CustomTypeSpec
                                             ([Identifier "System"],
                                              CustomType
                                                ((Identifier "Object"),[]))))]}] ""
            testCase "fully qualified type" <| fun _ -> 
              let source = " fun main (o : System::Object)
                        {
                            System::Console:.WriteLine(o);
                        }"
              Expect.equal (Compiler.Parser.parse source) [FunctionDeclaration
               {
                 Name = Identifier "main";
                 GenericParameters = [];
                 ReturnType = None;
                 Body = [StaticFunctionCallStatement
                     (CustomTypeSpec
                        ([Identifier "System"],
                         CustomType ((Identifier "Console"),[])),
                      FunctionCall
                        (Identifier "WriteLine",[],[IdentifierExpression (Identifier "o")]))]
                 Parameters = 
                  [(Identifier "o",
                    CustomTypeSpec
                      ([Identifier "System"],
                       CustomType ((Identifier "Object"),[])))];
                        }]""
        ]
