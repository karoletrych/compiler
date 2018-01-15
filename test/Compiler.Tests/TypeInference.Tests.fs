module Compiler.TypeInference.Tests
open Expecto
open Compiler.Parser
open Compiler.CompilerResult
open Compiler.TypeInference
open Compiler.Ast
open Compiler.ReferencedDllsMetadataRetriever
open Compiler.TypeResolving
open Compiler.TypeFinding

[<Tests>]
let tests =
  let infer src =
    let externals = getExternalTypes [System.Reflection.Assembly.GetAssembly(typeof<obj>)]
    [{Path = "test"; Code = src}]
    |> parseModules 
    >>= (fun modules -> resolve (modules, externals ))
    >>= (fun modules -> inferTypes (modules, find externals modules))



  testList "TypeInference.Tests" [
    testCase "type inference" <| fun _ ->
        let inference = infer "
            fun addone (x : int)
            {
                var result = x+1;
                var result2 = x+1.0;     //won't work
                return result;
            }"
            
        Expect.equal inference 
                (Result.failure 
                    (CannotInferBinaryExpressionType
                         ({Namespace = ["System"];
                       Name = "Int32";
                               GenericParameters = [];
                               DeclaringType = None;
                               },{
                        Namespace = ["System"];
                        Name = "Single";
                                   GenericParameters = [];
                                   DeclaringType = None;
                               })))""
    testCase "recursive type inference fails" <| fun _ ->
        let inference = infer "
                fun factorial (n : int) : int
                {
                    if n==0
                        return 1;
                    else
                        return n * factorial(n-1);
                }"
        Expect.equal inference 
            (Failure [FunctionTypeCannotBeInferred
                 ("factorial",[{Namespace = ["System"];
                                Name = "Int32";
                                            GenericParameters = [];
                                            DeclaringType = None;
                               }]);
               FunctionTypeCannotBeInferred
                 ("factorial",[{Namespace = ["System"];
                               Name = "Int32";
                                            GenericParameters = [];
                                            DeclaringType = None;
                               }])]) ""
    testCase "basic types" <| fun _ ->
        let inference = infer @"
                fun main 
                {
                    val name = ""Karol"";
                    val age = 22;
                    val weight = 65.1;
                    val arr = [1;2;3; ""string""; name; age; weight];
                }"
        Expect.isTrue (inference |> Result.isSuccess) ""
    testCase "classes" <| fun _ ->
        let inference = infer """
             class A
             {
             }
             class B : A
             {
             }
             class C : A
             {
             }

             fun testFunction
             {
                 val types = [
                  new System::Object();
                  new A();
                  new B();
                  new C();
                  "str";
                  42];
             }
             """
        Expect.isTrue (inference |> Result.isSuccess) ""
]