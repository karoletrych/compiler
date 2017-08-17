module Compiler.TypeInference.Tests

open Expecto
open Compiler.Ast
open Compiler.Parser

[<Tests>]
let tests =
  testList "TypeInference.Tests" [
    testCase "type inference works" <| fun _ ->
        let program = parse "
                fun addone (x : int)
                {
                    var result;
                    var result2;
                    result = x+1;
                    result2 = x+1.0;     //won't work
                    return result;
                }"
        Expect.equal program [] ""
    testCase "recursive type inference fails" <| fun _ ->
        let program = parse "
                fun factorial (n : int)
                {
                    if n==0
                        return 1;
                    else
                        return n * factorial(n-1);
                }"
        printfn "%A" program
        Expect.equal program [] ""
    testCase "basic types" <| fun _ ->
        let program = parse "
                fun main 
                {
                    val name = 'Karol';
                    val age = 22;
                    val height = 170.3;
                }"
        printfn "%A" program
        Expect.equal program [] ""
    ]