module Compiler.Parser.Tests.Expressions

open Expecto
open Compiler.Ast
open Compiler.Parser

[<Tests>]
let tests =
  testList "Parser.Tests.FunctionCalls" [
    testCase "factorial function is parsed" <| fun _ ->
      let source = "
        fun factorial (n :int)
        {
            if n==0
                return 1;
            else
                return n * factorial(n-1);
        }
        "
      Expect.equal (parse source) 
          [FunctionDeclaration
               (Identifier("factorial"), [(Identifier("n"), Int)], None,
                [IfStatement
                   (BinaryExpression
                      (IdentifierExpression(Identifier("n")),Equal,
                       LiteralExpression (IntLiteral 0)),
                    ReturnStatement (Some (LiteralExpression (IntLiteral 1))),
                    Some
                      (ReturnStatement
                         (Some
                            (BinaryExpression
                               (IdentifierExpression (Identifier("n")),Multiply,
                                FunctionCallExpression
                                  (Identifier("factorial"),
                                   [BinaryExpression
                                      (IdentifierExpression (Identifier("n")), Subtract,
                                       LiteralExpression (IntLiteral 1))]))))))])] ""
    testCase "multiple assignments and function calls in single statement work" <| fun _ ->
      let source = "
        fun main
        {
          a = n = x = foo(123, 5);
        }
        "
      Expect.equal (parse source) [FunctionDeclaration
       (Identifier("main"), [], None,
        [AssignmentStatement
           (IdentifierExpression( Identifier("a")),
            AssignmentExpression
              (IdentifierExpression(Identifier("n")),
               AssignmentExpression
                 (IdentifierExpression(Identifier("x")),
                  FunctionCallExpression
                    (Identifier("foo"),
                     [LiteralExpression (IntLiteral 123);
                      LiteralExpression (IntLiteral 5)]))))])]  ""
  ]
  
