module Compiler.Parser.Tests.FunctionCalls

open Expecto
open Compiler.Parser
open Compiler.Tests.ResultTestHelper

[<Tests>]
let tests =
  testList "Parser.Tests.FunctionCalls" [
    testCase "hello world" <| fun _ ->
      let source = @"
        fun main{print (""hello world!"");}
        "
      isOk (parseDeclarations source)  "print function call"
    testCase "hello world with spaces" <| fun _ ->
      let source = @"
        fun main
        
        {
          print (""hello world!"")   ;   
        }

        "
      isOk (parseDeclarations source) "print function call"                    
    testCase "function calls" <| fun _ ->
      let source = @"
        fun print (arg1 : string)
        {

        }

        fun main
        {
          print (""hello world!"");
        }
        "
      isOk (parseDeclarations source)  "print function call"                    
    testCase "function calls with explicit types" <| fun _ ->
      let source = @"
        fun internalPrint (arg1 : string) (arg2: int) : void
        {
          return pr(arg1);
        }

        fun print (arg1 : string)

        {
          internalPrint (arg1, count(arg1));
        }

        fun main
        
        {
          
          print (""hello world!"");   
        }

        "
      isOk (parseDeclarations source)  "print function call"               
    testCase "generic function call" <| fun _ ->
      let source = @"
          fun main{print<int,int,TMyType<int,float>>(""hello world!"");}
          "
      isOk (parseDeclarations source) ""
   ]
        