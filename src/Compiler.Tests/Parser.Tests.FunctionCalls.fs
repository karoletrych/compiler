module Parser.Tests.FunctionCalls

open Expecto
open Compiler.Ast
open Compiler.Parser

[<Tests>]
let tests =
  testList "Parser.Tests.FunctionCalls" [
    testCase "hello world" <| fun _ ->
      let source = "
        fun main{print ('hello world!');}
        "
      Expect.equal (Compiler.Parser.parse source) (
        [
          FunctionDeclaration("main", [], None,
            (
              [
                  ExpressionStatement(
                      FunctionCallExpression(
                        ("print"),
                        [LiteralExpression(StringLiteral("hello world!"))]))]))]) "print function call"
    testCase "hello world with spaces" <| fun _ ->
      let source = "
        fun main
        
        {
          
          print ('hello world!')   ;   
          
        }

        "
      Expect.equal (Compiler.Parser.parse source) (
        [
          FunctionDeclaration("main", [], None,
            (
              [
                  ExpressionStatement(
                      FunctionCallExpression(
                        ("print"),
                        [LiteralExpression(StringLiteral("hello world!"))]))]))]) "print function call"                    
    testCase "function calls" <| fun _ ->
      let source = "
        fun print arg1
        {

        }

        fun main
        
        {
          
          print ('hello world!')   ;   
          
        }

        "
      Expect.equal (Compiler.Parser.parse source) (
        [FunctionDeclaration
          ("print", [ScalarVariableDeclaration ("arg1",None, true)], None, []);
       FunctionDeclaration
         ("main", [], None,
          [ExpressionStatement
             (FunctionCallExpression
                ("print",[LiteralExpression (StringLiteral "hello world!")]))])]) "print function call"                    
    testCase "function calls with explicit types" <| fun _ ->
      let source = "
        fun internalPrint (arg1 : string) (arg2: int) : void
        {
          return pr(arg1);
        }

        fun print arg1

        {
          internalPrint (arg1, count(arg1));
        }

        fun main
        
        {
          
          print ('hello world!')   ;   
        }

        "
      Expect.equal (Compiler.Parser.parse source) (
        [
          FunctionDeclaration
         ("internalPrint",
          [ScalarVariableDeclaration ("arg1",Some String, true);
           ScalarVariableDeclaration ("arg2",Some Int, true)], Some Void,
          [ReturnStatement
             (Some
                (FunctionCallExpression
                   ("pr",[IdentifierExpression {Identifier = "arg1";}])))]);
           FunctionDeclaration
                   ("print", [ScalarVariableDeclaration ("arg1", None, true)], None,
                    [ExpressionStatement
                       (FunctionCallExpression
                          ("internalPrint",
                           [IdentifierExpression {Identifier = "arg1";};
                            FunctionCallExpression
                              ("count",[IdentifierExpression {Identifier = "arg1";}])]))]);
           FunctionDeclaration
                   ("main", [], None,
                    [ExpressionStatement
                       (FunctionCallExpression
                          ("print",[LiteralExpression (StringLiteral "hello world!")]))])] )
                          "print function call"               
  ]
        
