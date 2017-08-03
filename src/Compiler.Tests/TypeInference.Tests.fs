module TypeInference.Tests

open Expecto
open Compiler.Ast
open Compiler.Parser

[<Tests>]
let tests =
  testList "Parser.Tests.FunctionCalls" [
    testCase "type inference works" <| fun _ ->
        let program1 = parse "
                fun factorial n 
                {
                    if n==0
                        return 1;
                    else
                        return n * factorial(n-1);
                }"
        Expect.equal program1 [] ""
    testCase "type inference works" <| fun _ ->
        let program2 = parse "
                fun addone x 
                {
                    var result;
                    var result2;
                    result = x+1;
                    result2 = x+1.0;     //won't work
                    return result;
                }"
        Expect.equal program2 [] ""
    ]