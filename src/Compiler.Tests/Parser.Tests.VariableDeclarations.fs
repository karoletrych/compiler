module Parser.Tests.VariableDeclarations
open Expecto
open Compiler.Ast
open Compiler.Parser

[<Tests>]
let tests =
  testList "Parser.Tests.FunctionCalls" [
    testCase "explicit type variable declaration" <| fun _ ->
        let source = " fun main
                        {
                            val x : int;
                        }"
        Expect.equal (parse source) [FunctionDeclaration
   ("main", [], None,
    [VariableDeclarationStatement
       (ScalarVariableDeclaration ("x",Some Int,true))])]  ""
    testCase "explicit type variable declaration with assignment" <| fun _ ->
        let source = " fun main
                        {
                            val x : int = 4;
                        }"
        Expect.equal (parse source) [] ""
    testCase "implicit type variable declaration" <| fun _ ->
        let source = " fun main
                        {
                            val x;
                        }"
        Expect.equal (parse source) [FunctionDeclaration
           ("main", [], None,
            [VariableDeclarationStatement (ScalarVariableDeclaration ("x",None,true))])]  ""
    testCase "implicit type variable declaration with assignment" <| fun _ ->
        let source = " fun main
                        {
                            val x = 4;
                        }"
        Expect.equal (parse source) [FunctionDeclaration
           ("main", [], None,
            [VariableDeclarationStatement (ScalarVariableDeclaration ("x",None,true))])]  ""
    testCase "multiple variable declarations" <| fun _ ->
        let source = " fun main
                        {
                            val x : int;
                            var y : int = 4;
                            val z;
                            var a = 4;
                            val s1 = 'i''m a string variable';
                            var s2 : string = 'another string';
                            val f1 : float = 3.14;
                            var f2 : double = 3.141231;
                            val s3 : string;
                        }"
        Expect.equal (parse source) [] ""
     ]
 

//     testCase "2" <| fun _ ->
//       let source = "
//  fun foo a b
//  {
//      return a + b;
//  }

//  fun main
//  {
//      print (foo 1 2);
//  }
//         "
//       failtest "not implemented"
//         "
//       failtest "not implemented"
//     testCase "4" <| fun _ ->
//       let source = "
//  fun main
//  {
//      var i = 1;
//      while(i<100)
//      {
//          i = i + 1;
//          print i;
//      }
//  }
//         "
//       failtest "not implemented"

//     testCase "5" <| fun _ ->
//       let source = "
//  val f = 3.2
//  val i = 3
//  val s = \"hello\"

//  fun prnt variable // 'a -> void
//  {
//      print variable;
//  }

//  fun main
//  {
//      prnt f;
//      prnt i;
//      prnt s;
//  }
//         "
//       failtest "not implemented"
//  