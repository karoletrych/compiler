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
                  FunctionCallStatement(
                      (
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
                  FunctionCallStatement(
                      (
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
          ("print", ["arg1",None], None, []);
       FunctionDeclaration
         ("main", [], None,
          [FunctionCallStatement
             (
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
          [("arg1", Some String);
           ("arg2", Some Int)], Some Void,
          [ReturnStatement
             (Some
                (FunctionCallExpression
                   ("pr",[IdentifierExpression {Identifier = "arg1";}])))]);
           FunctionDeclaration
                   ("print", [("arg1", None)], None,
                    [FunctionCallStatement
                       (
                          ("internalPrint",
                           [IdentifierExpression {Identifier = "arg1";};
                            FunctionCallExpression
                              ("count",[IdentifierExpression {Identifier = "arg1";}])]))]);
           FunctionDeclaration
                   ("main", [], None,
                    [FunctionCallStatement
                       (
                          ("print",[LiteralExpression (StringLiteral "hello world!")]))])] )
                          "print function call"               
  ]
        
