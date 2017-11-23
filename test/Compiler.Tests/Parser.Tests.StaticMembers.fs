module Compiler.Parser.Tests.StaticMembers

open Compiler.Tests.ResultTestHelper
open Compiler.Parser
open Expecto

[<Tests>]
let tests = 
    testList "Parser.Tests.StaticMembers" 
        [ 
            testCase "static function call statement" <| fun _ -> 
              let source = " fun main
                        {
                            System::Console:.WriteLine();
                        }"
              isOk (parse source) "" 
            testCase "static function call expression" <| fun _ -> 
              let source = " fun main
                        {
                            var s = System::Console:.ReadLine();
                        }"
              isOk (parse source) "" 
            testCase "fully qualified type" <| fun _ -> 
              let source = " fun main
                        {
                            var s : System::Object;
                        }"
              isOk (parse source) ""
            testCase "fully qualified type" <| fun _ -> 
              let source = " fun main (o : System::Object)
                        {
                            System::Console:.WriteLine(o);
                        }"
              isOk (parse source) ""
        ]
