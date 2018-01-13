module Compiler.TypeResolving.Tests

open Expecto
open Compiler.Ast
open Compiler.Parser
open Compiler.CompilerResult
open Compiler.ReferencedDllsMetadataRetriever


let resolve src = 
    let externals = getExternalTypes [System.Reflection.Assembly.GetAssembly(typeof<obj>)]
    [{Path = "test"; Code = src}]
    |> parseModules 
    >>= (fun modules -> resolve (modules, externals))

[<Tests>]
let tests =
  testList "TypeResolving.Tests" [
    testCase "resolving valid System.Console.WriteLine call" <| fun _ ->
        let scanResult = 
            @"fun main {System::Console:.WriteLine(""Hello, world!"");}" 
            |> resolve
        Expect.equal scanResult (
                Result.succeed 
                    [{Identifier = {Namespace = [];
                          Name = "test";
                                      GenericParameters = [];
                                      DeclaringType = None;
                                      };
                    Functions = [{
                                   Name = "main";
                                   Parameters = [];
                                   ReturnType = None;
                                   Body = [
                                            StaticFunctionCallStatement(TypeIdentifier {Namespace = ["System"];
                                                        Name = "Console";
                                                         GenericParameters = [];
                                                         DeclaringType = None;
                                                         },{
                                                             Name = "WriteLine";
                                                             GenericArguments = [];
                                                             Arguments =
                                                             [AstExpression (LiteralExpression (StringLiteral "Hello, world!"))]
                                                  })];}]; Classes = [];}]) ""
    testCase "resolving invalid Sys.Cons.WriteL call" <| fun _ ->
        let scanResult = 
            @"fun main{Sys::Cons:.WriteL(""Hello, world!"");}"
            |> resolve
        Expect.equal scanResult (Result.failure (TypeNotFound (CustomTypeSpec (["Sys"],{Name = "Cons";
                                                     GenericArgs = [];})))) ""
        
    
    testCase "classes in the same module are found and substituted" <| fun _ ->
        let scanResult = 
            @"
                class A
                {
                }
                class B : A
                {
                    fun main
                    {
                        A:.Foo();
                    }
                }
            
            "
            |> resolve
        Expect.equal scanResult (Result.succeed 
                [{Identifier = {Namespace = [];
                                  Name = "test";
                                  GenericParameters = [];
                                  DeclaringType = None;
                                  };
        Functions = [];
        Classes =
                 [{Identifier = {Namespace = [];
                                 Name = "A";
                                 GenericParameters = []
                                 DeclaringType = None;
                                 };
                   BaseClass = None;
                   Fields = [];
                   Constructors = [];
                   Functions = [];};
                  {Identifier = {Namespace = [];
                                 Name = "B";
                                 GenericParameters = [];
                                 DeclaringType = None
                                 };
                   BaseClass = Some (TypeIdentifier {Namespace = [];
                                                     Name = "A";
                                                      GenericParameters = [];
                                                      DeclaringType = None
                                                     });
                   Fields = [];
                   Constructors = [];
                   Functions =
                    [{Name = "main";
                      Parameters = [];
                      ReturnType = None;
                      Body =
                       [StaticFunctionCallStatement
                          (TypeIdentifier {Namespace = [];
                                           Name = "A";
                                           GenericParameters = [];
                                           DeclaringType = None
                                           },
                           {Name = "Foo";
                            GenericArguments = [];
                            Arguments = [];})];}];}];}]) ""

    ]