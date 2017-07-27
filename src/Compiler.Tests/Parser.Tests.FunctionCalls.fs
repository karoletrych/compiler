module Tests

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
          ("print", [ScalarVariableDeclaration (None,"arg1")], None, []);
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
          [ScalarVariableDeclaration (Some String,"arg1");
           ScalarVariableDeclaration (Some Int,"arg2")], Some Void,
          [ReturnStatement
             (Some
                (FunctionCallExpression
                   ("pr",[IdentifierExpression {Identifier = "arg1";}])))]);
           FunctionDeclaration
                   ("print", [ScalarVariableDeclaration (None,"arg1")], None,
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
//     testCase "3" <| fun _ ->
//       let source = "
//  fun foo b
//  {
//      a = 3;       // readonly
//      return a * b;
//  }

//  fun main
//  {
//      x = foo 5;
//      print x
//  }
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
//   ]
