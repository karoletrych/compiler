module Compiler.TypeInference.Tests
open Expecto
open Compiler.Parser
open Compiler.CompilerResult
open Compiler.TypeIdentifiersFinding
open Compiler.TypeInference
open Compiler.Ast
open Compiler.ReferencedAssembliesMetadata
open Compiler.TypeResolving
open Compiler.TypeFinding

[<Tests>]
let tests =
  let infer src =
    let externals = externalTypes [System.Reflection.Assembly.GetAssembly(typeof<obj>)]
    [{Path = "test"; Code = src}]
    |> parseModules 
    >>= (fun modules -> resolve (modules, typeIdentifiers externals modules))
    >>= (fun modules -> inferTypes (modules, typesDictionary externals modules))



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
                       TypeName = {Name = ["Int32"];
                               GenericArguments = [];};},{
                        Namespace = ["System"];
                       TypeName = {Name = ["Single"];
                                   GenericArguments = [];};})))""
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
                                TypeName = {Name = ["Int32"];
                                            GenericArguments = [];};}]);
               FunctionTypeCannotBeInferred
                 ("factorial",[{Namespace = ["System"];
                                TypeName = {Name = ["Int32"];
                                            GenericArguments = [];};}])]) ""
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
        let inference = infer @"
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
                  new obj();
                  new A();
                  new B();
                  new C();
                  ""str"";
                  42];
             }
             "
        Expect.isTrue (inference |> Result.isSuccess) ""
]