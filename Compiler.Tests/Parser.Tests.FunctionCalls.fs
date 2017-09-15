module Compiler.Parser.Tests.FunctionCalls

open Expecto
open Compiler.Ast
open Compiler.Parser

[<Tests>]
let tests =
  testList "Parser.Tests.FunctionCalls" [
    testCase "hello world" <| fun _ ->
      let source = @"
        fun main{print (""hello world!"");}
        "
      Expect.equal (Compiler.Parser.parse source) (
        [
          FunctionDeclaration(Identifier("main"),[], [], None,
            (
              [
                  FunctionCallStatement(
                      (
                        FunctionCall(
                          (Identifier("print")),[],
                          [LiteralExpression(StringLiteral("hello world!"))])))]))]) "print function call"
    testCase "hello world with spaces" <| fun _ ->
      let source = @"
        fun main
        
        {
          
          print (""hello world!"")   ;   
          
        }

        "
      Expect.equal (Compiler.Parser.parse source) (
        [
          FunctionDeclaration(Identifier("main"),[], [], None,
            (
              [
                  FunctionCallStatement(
                      (FunctionCall (Identifier("print"),[], [LiteralExpression(StringLiteral("hello world!"))])))]))]) "print function call"                    
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
      Expect.equal (Compiler.Parser.parse source) (
        [FunctionDeclaration
          (Identifier("print"),[], [Identifier("arg1"), String], None, []);
       FunctionDeclaration
         (Identifier("main"),[], [], None,
          [FunctionCallStatement
             ( FunctionCall
                (Identifier("print"),[],[LiteralExpression (StringLiteral "hello world!")]))])]) "print function call"                    
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
      Expect.equal (Compiler.Parser.parse source) (
        [
          FunctionDeclaration
         (Identifier("internalPrint"),[],
          [(Identifier("arg1"), String);
           (Identifier("arg2"), Int)], Some Void,
          [ReturnStatement
             (Some
                (FunctionCallExpression
                   (FunctionCall (Identifier("pr"),[],[IdentifierExpression (Identifier("arg1"))]))))]);
                   
           FunctionDeclaration
                   (Identifier("print"),[], [(Identifier("arg1"), String)], None,
                    [FunctionCallStatement
                       (
                         FunctionCall
                          (Identifier("internalPrint"),[],
                           [IdentifierExpression (Identifier("arg1"));
                            FunctionCallExpression
                              (FunctionCall(Identifier("count"),[],[IdentifierExpression (Identifier("arg1"))]))
                              ]))]);
           FunctionDeclaration
                   (Identifier("main"),[], [], None,
                    [FunctionCallStatement
                       ((FunctionCall
                          (Identifier("print"),[],[LiteralExpression (StringLiteral "hello world!")])))])] )
                          "print function call"               
    testCase "generic function call" <| fun _ ->
      let source = @"
          fun main{print<int,int,TMyType<int,float>>(""hello world!"");}
          "
      Expect.equal (parse source) [FunctionDeclaration
         (Identifier "main",[], [], None,
          [FunctionCallStatement
             (FunctionCall
                (Identifier "print",
                 [Int; Int;
                  CustomTypeSpec
                    ([], GenericCustomTypeSpec
                       (SimpleTypeSpec (Identifier "TMyType"),[Int; Float]))],
                 [LiteralExpression (StringLiteral "hello world!")]))])] ""
   ]
        