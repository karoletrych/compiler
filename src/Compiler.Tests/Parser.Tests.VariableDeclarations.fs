module Parser.Tests.VariableDeclarations

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
                        ("main", [], None, 
                         [ VariableDeclaration
                               ("x", Some Int, (Some (LiteralExpression(IntLiteral(4))) )) ]) ] ""
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
                        ("main", [], None, 
                         [( (ValueDeclaration("x", None, LiteralExpression(IntLiteral(4))) )) ]) ] ""
          
          testCase "explicit type variable declaration" 
          <| fun _ -> 
              let source = " fun main
                        {
                            var x : int;
                        }"
              Expect.equal (parse source) 
                  [ FunctionDeclaration
                        ("main", [], None, 
                         [ (VariableDeclaration("x", Some Int, None)) ]) ] ""
          
          testCase "explicit type variable declaration with assignment" 
          <| fun _ -> 
              let source = " fun main
                        {
                            var x : int = 4;
                        }"
              Expect.equal (parse source) 
                  [ FunctionDeclaration
                        ("main", [], None, 
                         [ 
                               (VariableDeclaration("x", Some Int, (Some(LiteralExpression(IntLiteral(4)))))) ]) ] 
                  ""
          
          testCase "implicit type variable declaration" 
          <| fun _ -> 
              let source = " fun main
                        {
                            var x;
                        }"
              Expect.equal (parse source) 
                  [ FunctionDeclaration
                        ("main", [], None, [ (VariableDeclaration("x", None, None)) ]) ] 
                  ""
          
          testCase "implicit type variable declaration with assignment" 
          <| fun _ -> 
              let source = " fun main
                        {
                            var x = 4;
                        }"
              Expect.equal (parse source) 
                  [ FunctionDeclaration
                        ("main", [], None, 
                         [ 
                               (VariableDeclaration("x", None, Some(LiteralExpression(IntLiteral(4))))) ]) ] ""
          
          testCase "multiple variable declarations" 
          <| fun _ -> 
              let source = " fun main
                        {
                            var y : int = 4;
                            var a = 4;
                            val s1 = 'im a string variable';
                            var s2 : string = 'another string';
                            val f1 : float = 3.14;
                            var f2 : double = 3.141231;
                        }"
              Expect.equal (parse source) 
                  [ FunctionDeclaration
                        ("main", [], None, 
                         [ 
                               (VariableDeclaration("y", Some Int, Some(LiteralExpression(IntLiteral 4))))
                           
                           
                               (VariableDeclaration("a", None, Some(LiteralExpression(IntLiteral 4))))
                           
                           
                               (ValueDeclaration
                                    ("s1", None, LiteralExpression(StringLiteral "im a string variable")))
                           
                           
                               (VariableDeclaration
                                    ("s2", Some String, Some(LiteralExpression(StringLiteral "another string"))))
                           
                           
                               (ValueDeclaration("f1", Some Float, LiteralExpression(FloatLiteral 3.14)))
                           
                           
                               (VariableDeclaration
                                    ("f2", Some Double, Some(LiteralExpression(FloatLiteral 3.141231)))) ]) ] "" ]