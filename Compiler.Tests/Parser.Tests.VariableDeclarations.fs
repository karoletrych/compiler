module Compiler.Parser.Tests.VariableDeclarations

open Compiler.Ast
open Compiler.Parser
open Expecto

[<Tests>]
let tests = 
    testList "Parser.Tests.FunctionCalls" 
        [ testCase "explicit type value declaration" <| fun _ -> 
              let source = " fun main
                        {
                            val x : int;
                        }"
              Expect.throws (fun () -> parse source |> ignore) "value must be assigned"
          
          testCase "explicit type value declaration with assignment" 
          <| fun _ -> 
              let source = " fun main
                        {
                            val x : int = 4;
                        }"
              Expect.equal (parse source) 
                  [ FunctionDeclaration
                        (Identifier("main"),[], [], None, 
                         [ ValueDeclaration
                               (Identifier("x"), Some Int, ((LiteralExpression(IntLiteral(4))) )) ]) ] ""
          testCase "implicit type value declaration" <| fun _ -> 
              let source = " fun main
                        {
                            val x;
                        }"
              Expect.throws (fun () -> parse source |> ignore) "value must be assigned"
          
          testCase "implicit type value declaration with assignment" 
          <| fun _ -> 
              let source = " fun main
                        {
                            val x = 4;
                        }"
              Expect.equal (parse source) 
                  [ FunctionDeclaration
                        (Identifier("main"),[], [], None, 
                         [( (ValueDeclaration(Identifier("x"), None, LiteralExpression(IntLiteral(4))) )) ]) ] ""
          
          testCase "explicit type variable declaration" 
          <| fun _ -> 
              let source = " fun main
                        {
                            var x : int;
                        }"
              Expect.equal (parse source) 
                  [ FunctionDeclaration
                        (Identifier("main"),[], [], None, 
                         [ (VariableDeclaration(DeclarationWithType( Identifier("x"), Int))) ]) ] ""
          
          testCase "explicit type variable declaration with assignment" 
          <| fun _ -> 
              let source = " fun main
                        {
                            var x : int = 4;
                        }"
              Expect.equal (parse source) 
                  [ FunctionDeclaration
                        (Identifier("main"),[], [], None, 
                         [ 
                               (VariableDeclaration(
                                   FullDeclaration(Identifier("x"), Int, ((LiteralExpression(IntLiteral(4))))))) ]) ] 
                  ""
          
          testCase "implicit type variable declaration" 
          <| fun _ -> 
              let source = " fun main
                        {
                            var x;
                        }"
              Expect.throws (fun () -> (parse source) |> ignore) "illegal"
          
          testCase "implicit type variable declaration with assignment" 
          <| fun _ -> 
              let source = " fun main
                        {
                            var x = 4;
                        }"
              Expect.equal (parse source) 
                  [ FunctionDeclaration
                        (Identifier("main"),[], [], None, 
                         [ 
                               (VariableDeclaration(DeclarationWithInitialization(Identifier("x"), (LiteralExpression(IntLiteral(4)))))) ]) ] ""
          
          testCase "multiple variable declarations" 
          <| fun _ -> 
              let source = @" fun main
                        {
                            var y : int = 4;
                            var a = 4;
                            val s1 = ""im a string variable"";
                            var s2 : string = ""another string"";
                            val f1 : float = 3.14;
                            var f2 : double = 3.141231;
                        }"
              Expect.equal (parse source) 
                  [ FunctionDeclaration
                        (Identifier("main"),[], [], None, 
                         [ 
                               (VariableDeclaration(FullDeclaration(Identifier("y"), Int, (LiteralExpression(IntLiteral 4)))))
                           
                           
                               (VariableDeclaration(DeclarationWithInitialization(Identifier("a"), (LiteralExpression(IntLiteral 4)))))
                           
                           
                               (ValueDeclaration
                                    (Identifier("s1"), None, LiteralExpression(StringLiteral "im a string variable")))
                           
                           
                               (VariableDeclaration 
                               (FullDeclaration(Identifier("s2"), String, (LiteralExpression(StringLiteral "another string")))))
                           
                           
                               (ValueDeclaration(Identifier("f1"), Some Float, LiteralExpression(FloatLiteral 3.14)))
                           
                           
                               (VariableDeclaration(FullDeclaration
                                    (Identifier("f2"), Double, (LiteralExpression(FloatLiteral 3.141231))))) 
                            ]) ] "" ]