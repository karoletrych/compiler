module Compiler.TypeInference.Tests

// open Expecto
// open Compiler.Ast
// open Compiler.Parser

// [<Tests>]
// let tests =
//   testList "TypeInference.Tests" [
//     testCase "type inference works" <| fun _ ->
//         let program = parse "
//                 fun addone (x : int)
//                 {
//                     var result = x+1;
//                     var result2 = x+1.0;     //won't work
//                     return result;
//                 }"
//         Expect.equal program [] ""
//     testCase "recursive type inference fails" <| fun _ ->
//         let program = parse "
//                 fun factorial (n : int)
//                 {
//                     if n==0
//                         return 1;
//                     else
//                         return n * factorial(n-1);
//                 }"
//         printfn "%A" program
//         Expect.equal program [] ""
//     testCase "basic types" <| fun _ ->
//         let program = parse "
//                 fun main 
//                 {
//                     val name = 'Karol';
//                     val age = 22;
//                     val height = 170.3;
//                 }"
//         printfn "%A" program
//         Expect.equal program [] ""
//     ]



type T = {
    Base : T list;
    Name : string;
}
let o = { Base = []; Name = "Object"}
let str = {Base = [o]; Name = "String"}
let int = {Base = [o]; Name = "int"}
let exc = { Base = [o]; Name = "exception"}
let runtimeException = {
    Base = [exc]; Name = "RuntimeException"
    }
let anotherException = {
    Base = [exc]; Name = "AnotherException"
    }
let iface = {
    Base = [o]; Name = "Interface"
    }
let classA = {
    Base = [o;iface]; Name = "A"
    }

let classB = {
    Base = [o;iface]; Name = "B"
    }

let allTypes = [
 o;
 exc;
 runtimeException;
 str;
 int;
 anotherException
 ]

 
let program = parse @"
                fun main 
                {
                    val name = 'Karol';
                    val age = 22;
                    val weight = 65.1;
                    val arr = [1;2;3;""string""]
                }"

program;;