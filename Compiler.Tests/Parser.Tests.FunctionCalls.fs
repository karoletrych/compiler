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
          FunctionDeclaration(
            {
            Name = Identifier("main");
            GenericParameters = [];
            Parameters = [];
            ReturnType = None;
            Body = 
              [
                  FunctionCallStatement(
                      (
                        FunctionCall(
                          (Identifier("print")),[],
                          [LiteralExpression(StringLiteral("hello world!"))])))]})]) "print function call"
    testCase "hello world with spaces" <| fun _ ->
      let source = @"
        fun main
        
        {
          print (""hello world!"")   ;   
        }

        "
      Expect.equal (Compiler.Parser.parse source) (
        [
          FunctionDeclaration(
            {
            Name = Identifier("main");
            GenericParameters = [];
            Parameters = [];
            ReturnType = None;
            Body = 
              [
                  FunctionCallStatement(
                      (FunctionCall (Identifier("print"),[], [LiteralExpression(StringLiteral("hello world!"))])))]})]) "print function call"                    
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
          {
            Name = Identifier("print");GenericParameters = []; Parameters=[(Identifier("arg1"), String)]; ReturnType = None; Body = []};
       FunctionDeclaration
         { Name = Identifier("main"); GenericParameters = []; Parameters = []; ReturnType = None;
         Body = 
          [FunctionCallStatement
             ( FunctionCall
                (Identifier("print"),[],[LiteralExpression (StringLiteral "hello world!")]))]}]) "print function call"                    
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
         {
          Name = Identifier("internalPrint"); GenericParameters = [];
          Parameters = [(Identifier("arg1"), String);
                        (Identifier("arg2"), Int)]; ReturnType = Some Void;
          Body = 
          [
              ReturnStatement
               (Some
                  (FunctionCallExpression
                     (FunctionCall (Identifier("pr"),[],[IdentifierExpression (Identifier("arg1"))]))))]};
                     
             FunctionDeclaration
                     {Name = Identifier("print");
                     GenericParameters = [];
                     Parameters = [(Identifier("arg1"), String)];
                     ReturnType = None;
                     Body = [FunctionCallStatement
                         (
                           FunctionCall
                            (Identifier("internalPrint"),[],
                             [IdentifierExpression (Identifier("arg1"));
                              FunctionCallExpression
                                (FunctionCall(Identifier("count"),[],[IdentifierExpression (Identifier("arg1"))]))
                                ]))]};
             FunctionDeclaration
                     {Name = Identifier("main");GenericParameters = [];Parameters = [];ReturnType= None;
                      Body = [FunctionCallStatement
                             ((FunctionCall
                                (Identifier("print"),[],[LiteralExpression (StringLiteral "hello world!")])))]}] )
                          "print function call"               
    testCase "generic function call" <| fun _ ->
      let source = @"
          fun main{print<int,int,TMyType<int,float>>(""hello world!"");}
          "
      Expect.equal (parse source) [FunctionDeclaration
         {
         Name = Identifier "main";
         GenericParameters = [];
         Parameters = [];
         ReturnType = None;
          Body = [FunctionCallStatement
             (FunctionCall
                (Identifier "print",
                 [Int; Int;
                  CustomTypeSpec
                    ([], CustomType
                       ((Identifier "TMyType"),[Int; Float]))],
                 [LiteralExpression (StringLiteral "hello world!")]))]}] ""
   ]
        