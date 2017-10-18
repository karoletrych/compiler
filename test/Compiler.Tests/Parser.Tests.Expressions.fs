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
      Expect.isOk (parse source) ""
    testCase "multiple assignments and function calls in single statement work" <| fun _ ->
      let source = "
        fun main
        {
          a = n = x = foo(123, 5);
        }
        "
      Expect.isOk (parse source) ""
  ]
  
