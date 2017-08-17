module Compiler.Parser.Tests.Classes

open Expecto
open Compiler.Ast
open Compiler.Parser

[<Tests>]
let tests =
  testList "Parser.Tests.Classes" [
    testCase "empty class" <| fun _ ->
      let source = "
        class A
        {
        }
        "
      Expect.equal (parse source) 
          [] ""
    testCase "class with fields" <| fun _ ->
      let source = "
        class A
        {
            val a : A = new A();
            var i : int = 3;
            var f : float = 3;
        }
        "
      Expect.equal (parse source) 
        [] ""
    testCase "class with functions" <| fun _ ->
      let source = "
        class A
        {
            var i : int;

            fun increment
            {
                i = i + 1;
            }

            fun getI
            {
                return i;
            }
        }
        class B {
            fun useClassA
            {
                val a = new A();
                a.increment();
                a.increment();
                a.increment();
                if a.getI() == 3
                    return true;
                return false;
            }
        }
        "
      Expect.equal (parse source) 
          [] ""
   ]
  
