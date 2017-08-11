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
                      (IdentifierExpression("n"),Equal,
                       LiteralExpression (IntLiteral 0)),
                    ReturnStatement (Some (LiteralExpression (IntLiteral 1))),
                    Some
                      (ReturnStatement
                         (Some
                            (BinaryExpression
                               (IdentifierExpression "n",Multiply,
                                FunctionCallExpression
                                  ("factorial",
                                   [BinaryExpression
                                      (IdentifierExpression "n", Subtract,
                                       LiteralExpression (IntLiteral 1))]))))))])] ""
    testCase "multiple assignments and function calls in single statement work" <| fun _ ->
      let source = "
        fun main
        {
          a = n = x = foo(123, 5);
        }
        "
      Expect.equal (parse source) [FunctionDeclaration
       ("main", [], None,
        [AssignmentStatement
           ("a",
            AssignmentExpression
              ("n",
               AssignmentExpression
                 ("x",
                  FunctionCallExpression
                    ("foo",
                     [LiteralExpression (IntLiteral 123);
                      LiteralExpression (IntLiteral 5)]))))])]  ""
  ]
  
