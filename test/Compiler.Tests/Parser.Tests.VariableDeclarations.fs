module Compiler.Parser.Tests.VariableDeclarations

open Compiler.Parser
open Compiler.Tests.ResultTestHelper
open Expecto

[<Tests>]
let tests = 
    testList "Parser.Tests.FunctionCalls" 
        [ testCase "explicit type value declaration" <| fun _ -> 
              let source = " fun main
                        {
                            val x : int;
                        }"
              isError (parse source) "value must be assigned"
          
          testCase "explicit type value declaration with assignment" 
          <| fun _ -> 
              let source = " fun main
                        {
                            val x : int = 4;
                        }"
              isOk (parse source) ""
          testCase "implicit type value declaration" <| fun _ -> 
              let source = " fun main
                        {
                            val x;
                        }"
              isError (parse source) "value must be assigned"
          
          testCase "implicit type value declaration with assignment" 
          <| fun _ -> 
              let source = " fun main
                        {
                            val x = 4;
                        }"
              isOk (parse source) ""
          
          testCase "explicit type variable declaration" 
          <| fun _ -> 
              let source = " fun main
                        {
                            var x : int;
                        }"
              isOk (parse source) ""
          
          testCase "explicit type variable declaration with assignment" 
          <| fun _ -> 
              let source = " fun main
                        {
                            var x : int = 4;
                        }"
              isOk (parse source) ""
          
          testCase "implicit type variable declaration" 
          <| fun _ -> 
              let source = " fun main
                        {
                            var x;
                        }"
              isError (parse source) "illegal"
          
          testCase "implicit type variable declaration with assignment" 
          <| fun _ -> 
              let source = " fun main
                        {
                            var x = 4;
                        }"
              isOk (parse source) ""
          
          testCase "multiple variable declarations" 
          <| fun _ -> 
              let source = @" fun main
                        {
                            var y : int = 4;
                            var a = 4;
                            val s1 = ""im a string variable"";
                            var s2 : string = ""another string"";
                            val f1 : float = 3.14;
                            var f2 : double = 3.141231;
                        }"
              isOk (parse source) ""
          testCase "array declaration with assignment" 
          <| fun _ -> 
              let source = @" fun main
                        {
                            var arr1 = [1;2;3;4.0;5.2;""six""; new System::Object()];
                            var arr2 = [[1;2];[1;""asd""]];
                        }"
              isOk (parse source) ""
]