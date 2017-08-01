module Parser.Tests.Expressions

open Expecto
open Compiler.Ast
open Compiler.Parser

[<Tests>]
let tests =
  testList "Parser.Tests.FunctionCalls" [
    testCase "factorial function is parsed" <| fun _ ->
      let source = "
        fun factorial n 
        {
            if n==0
                return 1;
            else
                return n * factorial(n-1);
        }
        "
      Expect.equal (parse source) 
          [FunctionDeclaration
               ("factorial", [("n", None)], None,
                [IfStatement
                   (BinaryExpression
                      (IdentifierExpression {Identifier = "n";},Equal,
                       LiteralExpression (IntLiteral 0)),
                    ReturnStatement (Some (LiteralExpression (IntLiteral 1))),
                    Some
                      (ReturnStatement
                         (Some
                            (BinaryExpression
                               (IdentifierExpression {Identifier = "n";},Multiply,
                                FunctionCallExpression
                                  ("factorial",
                                   [BinaryExpression
                                      (IdentifierExpression {Identifier = "n";},Subtract,
                                       LiteralExpression (IntLiteral 1))]))))))])] ""
  ]