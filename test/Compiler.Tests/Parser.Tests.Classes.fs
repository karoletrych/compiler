module Compiler.Parser.Tests.Classes

open Expecto
open Compiler.Tests.ResultTestHelper
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
      isOk (parse source) ""
    testCase "class with fields" <| fun _ ->
      let source = "
        class A
        {
            val a : A = new A()
            var i : int = 3
            var f : float = 3
        }
        "
      isOk (parse source) ""
    testCase "class with functions" <| fun _ ->
      let source = "
        class A
        {
            var i : int

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
      isOk (parse source) ""
    

    testCase "class with base constructor call" <| fun _ ->
      let source = "class B
                    {
                        val _tValue : T
                        val _vValue : V

                        construct (tValue : T) (vValue : V)
                        {
                            _tValue = tValue;
                            _vValue = vValue;
                        }
                    }
                    class A : B
                    {
                        val _tValue : T
                        val _number : int

                        construct (tValue : T) (number : int) : (tValue, number)
                        {
                            _tValue = tValue;
                            _number = number;
                        }
                    }"
      isOk (parse source) ""
    testCase "class extending class" <| fun _ ->
      let source = "class B
    {
        val _tValue : T
        val _vValue : V

        construct (tValue : T) (vValue : V)
        {
            _tValue = tValue;
            _vValue = vValue;
        }
    }
    class A : B
    {
        val _tValue : T
        val _number : int

        construct (tValue : T) (number : int) : (tValue, number)
        {
            _tValue = tValue;
            _number = number;
        }
    }"
      isOk (parse source) ""
    testCase "function declaration" <| fun _ ->
      let source = "
          fun add (t : T) (v : V) (u : U)
          {
            return t+v+u;
          }
        "
      isOk (parse source) ""
    ]

