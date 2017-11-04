module Compiler.TypeInference.Tests
open Expecto
open Compiler.Parser
open Compiler.CompilerResult
open Compiler.TypeFinding
open Compiler.TypeInference
open Compiler.Ast
open Compiler.Tests.ResultTestHelper

[<Tests>]
let tests =
  let infer program =
    parseDeclarations program
    |> Result.map Module.createDefault
    |> Result.map ( fun ast -> allKnownTypes ast |> (fun types -> inferTypes types ast) )
    |> Result.get
    |> fst
  testList "TypeInference.Tests" [
    testCase "type inference" <| fun _ ->
        let inference = infer "
            fun addone (x : int)
            {
                var result = x+1;
                var result2 = x+1.0;     //won't work
                return result;
            }"
            
        Expect.equal inference.Classes
         [{
          Name = "DEFAULT";
          GenericTypeParameters = [];
          BaseClass = None;
          ImplementedInterfaces = [];
          Properties = [];
          Constructor = None;
          FunctionDeclarations =
           [{Name = "addone";
             GenericParameters = [];
             Parameters = [("x", BuiltInTypeSpec Int)];
             ReturnType = None;
             Body =
              [VariableDeclaration
                 (DeclarationWithInitialization
                    ("result",
                     ExpressionWithInferredType
                       (BinaryExpression
                          (IdentifierExpression "x",Plus,
                           LiteralExpression (IntLiteral 1)),Some "System.Int32")));
               VariableDeclaration
                 (DeclarationWithInitialization
                    ("result2",
                     ExpressionWithInferredType
                       (BinaryExpression
                          (IdentifierExpression "x",Plus,
                           LiteralExpression (FloatLiteral 1.0)),None)));
               ReturnStatement
                 (Some
                    (ExpressionWithInferredType
                       (IdentifierExpression "result",Some "System.Int32")))];}];}] ""
    testCase "recursive type inference fails" <| fun _ ->
        let inference = infer "
                fun factorial (n : int)
                {
                    if n==0
                        return 1;
                    else
                        return n * factorial(n-1);
                }"
        Expect.equal inference.Classes
            [{Name = "DEFAULT";
              GenericTypeParameters = [];
              BaseClass = None;
              ImplementedInterfaces = [];
              Properties = [];
              Constructor = None;
              FunctionDeclarations =
               [{Name = "factorial";
                 GenericParameters = [];
                 Parameters = [("n", BuiltInTypeSpec Int)];
                 ReturnType = None;
                 Body =
                  [IfStatement
                     (BinaryExpression
                        (IdentifierExpression "n",Equal,LiteralExpression (IntLiteral 0)),
                      ReturnStatement
                        (Some
                           (ExpressionWithInferredType
                              (LiteralExpression (IntLiteral 1),Some "System.Int32"))),
                      Some
                        (ReturnStatement
                           (Some
                              (ExpressionWithInferredType
                                 (BinaryExpression
                                    (IdentifierExpression "n",Multiplication,
                                     FunctionCallExpression
                                       {Name = "factorial";
                                        GenericArguments = [];
                                        Arguments =
                                         [BinaryExpression
                                            (IdentifierExpression "n",Minus,
                                             LiteralExpression (IntLiteral 1))];}),None)))))];}];}] ""
    testCase "basic types" <| fun _ ->
        let inference = infer @"
                fun main 
                {
                    val name = ""Karol"";
                    val age = 22;
                    val weight = 65.1;
                    val arr = [1;2;3; ""string""; name; age; weight];
                }"
        Expect.equal inference.Classes [
            {
              Name = "DEFAULT";
              GenericTypeParameter = [];
              BaseClass = None;
              ImplementedInterfaces = [];
              Properties = [];
              Constructor = None;
              FunctionDeclarations =
               [{Name = "main";
                 GenericParameters = [];
                 Parameters = [];
                 ReturnType = None;
                 Body =
                  [ValueDeclaration
                     ("name", None,
                      ExpressionWithInferredType
                        (LiteralExpression (StringLiteral "Karol"),Some "System.String"));
                   ValueDeclaration
                     ("age", None,
                      ExpressionWithInferredType
                        (LiteralExpression (IntLiteral 22),Some "System.Int32"));
                   ValueDeclaration
                     ("weight", None,
                      ExpressionWithInferredType
                        (LiteralExpression (FloatLiteral 65.1),Some "System.Single"));
                   ValueDeclaration
                     ("arr", None,
                      ExpressionWithInferredType
                        (ListInitializerExpression
                           [LiteralExpression (IntLiteral 1);
                            LiteralExpression (IntLiteral 2);
                            LiteralExpression (IntLiteral 3);
                            LiteralExpression (StringLiteral "string");
                            IdentifierExpression "name"; IdentifierExpression "age";
                            IdentifierExpression "weight"],Some "System.Object"))];}];}]  ""
    testCase "classes" <| fun _ ->
        let inference = infer @"
             class A
             {
             }
             class B extends A
             {
             }
             class C extends A
             {
             }

             fun testFunction
             {
                 val types = [
                  new obj();
                  new A();
                  new B();
                  new C();
                  ""str"";
                  42];
             }
             "
        Expect.equal inference.Classes [] ""
]