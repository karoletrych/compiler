module Compiler.TypeInference.Tests
open Expecto
open Compiler.Parser

[<Tests>]
let tests =
  testList "TypeInference.Tests" [
    testCase "type inference works" <| fun _ ->
        let program = parse "
                fun addone (x : int)
                {
                    var result = x+1;
                    var result2 = x+1.0;     //won't work
                    return result;
                }"
        Expect.equal program [] ""
    // testCase "recursive type inference fails" <| fun _ ->
    //     let program = parse "
    //             fun factorial (n : int)
    //             {
    //                 if n==0
    //                     return 1;
    //                 else
    //                     return n * factorial(n-1);
    //             }"
    //     printfn "%A" program
    //     Expect.equal program [] ""
    // testCase "basic types" <| fun _ ->
    //     let program = parse @"
    //             fun main 
    //             {
    //                 val name = 'Karol';
    //                 val age = 22;
    //                 val weight = 65.1;
    //                 val arr = [1;2;3;""string""];
    //             }"
    //     printfn "%A" program
    //     Expect.equal program [] ""
//     testCase "classes" <| fun _ ->
//         let program = parse @"
//  class A
//  {
//  }
//  class B extends A
//  {
//  }
//  class C extends A
//  {
//  }

//  fun testFunction
//  {
//      val types = [
//       new obj();
//       new A();
//       new B();
//       new C();
//       ""str"";
//       42];
//  }
//  "
//         printfn "%A" program
//         Expect.equal program [] ""
    ]