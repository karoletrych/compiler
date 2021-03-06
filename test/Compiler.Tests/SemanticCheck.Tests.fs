module Compiler.SemanticCheck.Tests

open Expecto
open Compiler.Parser
open Compiler.CompilerResult
open Compiler.TypeResolving
open Compiler.ReferencedDllsMetadataRetriever
open Compiler.TypeInference
open Compiler.TypeFinding
open Compiler.SemanticCheck
open Compiler.Ast


let semanticCheck src = 
    let externals = getExternalTypes [System.Reflection.Assembly.GetAssembly(typeof<obj>)]
    [{Path = "test"; Code = src}]
    |> parseModules 
    >>= (fun modules -> resolve (modules, externals))
    >>= (fun modules -> inferTypes (modules, find externals modules))
    >>= (check true)

[<Tests>]
let tests =
    testList "SemanticCheck.Tests" [
        testCase "non boolean type in if/while statements" <| fun _ ->
            let semanticCheckResult = 
                @"
                fun main 
                {
                    while(3)
                    {

                    }
                    if(""true"")
                        return 3;
                    else 
                        return4;
                }
                "
                |> semanticCheck
            Expect.equal 
                semanticCheckResult 
                (Failure  [
                    NonBooleanExpressionInIfStatement {Namespace = ["System"];
                                          Name = "String";
                                                      GenericParameters = [];
                                                      DeclaringType = None;
                                                     }
                    NonBooleanExpressionInWhileStatement {Namespace = ["System"];
                                             Name = "Int32";
                                                     GenericParameters = [];
                                                     DeclaringType = None;
                                                     };
                   ]) ""
        testCase "assignment of invalid type in variable declaration" <| fun _ ->
            let semanticCheckResult = 
                @"
                fun main 
                {
                    var a : string = 3;
                    val b : int = 4.0;
                }
                "
                |> semanticCheck
            Expect.equal 
                semanticCheckResult 
                (Failure  [InvalidType ("b",{Namespace = ["System"];
                     Name = "Int32";
                     GenericParameters = [];
                     DeclaringType = None;
                     },
                                            {Namespace = ["System"];
                                             Name = "Single";
                                             GenericParameters = [];
                                             DeclaringType = None;
                                             });
        InvalidType ("a",{Namespace = ["System"];
                     Name = "String";
                     GenericParameters = [];
                     DeclaringType = None;
                                             },{Namespace = ["System"];
                                             Name = "Int32";
                                             GenericParameters = [];
                                             DeclaringType = None;
                                             })]) ""
        testCase "declaration with initializer of incompatible type" <| fun _ ->
            let semanticCheckResult = 
                @"
                fun main 
                {
                    val a : string = 3;
                    val b : int = 4.0;
                }
                "
                |> semanticCheck
            Expect.equal 
                semanticCheckResult 
                (Failure  [InvalidType ("b",{Namespace = ["System"];
                     Name = "Int32";
                     GenericParameters = [];
                     DeclaringType = None;
                                             },{Namespace = ["System"];
                                             Name = "Single";
                                             GenericParameters = [];
                                             DeclaringType = None;
                                             });
        InvalidType ("a",{Namespace = ["System"];
                     Name = "String";
                     GenericParameters = [];
                     DeclaringType = None;
                                             },{Namespace = ["System"];
                                             Name = "Int32";
                                             GenericParameters = [];
                                             DeclaringType = None;
                                             })]) ""
        testCase "assignment to readonly variable" <| fun _ ->
            let semanticCheckResult = 
                @"
                fun main 
                {
                    val a : string = ""123"";
                    a = a + ""321"";
                }
                "
                |> semanticCheck
            Expect.equal 
                semanticCheckResult 
                (Failure [AssignmentToReadOnlyVariable "a"]) ""                                   
        testCase "assignment to readonly field" <| fun _ ->
            let semanticCheckResult = 
                @"
                class A
                {
                    val b : string = ""BBB""

                    fun setB(arg : string)
                    {
                        b = arg;
                    }
                }

                fun main 
                {
                    val a = new A();
                    a.b = ""CCC"";
                    a.setB(""CCC"");
                }
                "
                |> semanticCheck
            Expect.equal 
                semanticCheckResult 
                (Failure[AssignmentToReadOnlyField
     ({Namespace = [];
       Name = "A";
       GenericParameters = [];
       DeclaringType = Some {Namespace = [];
                             Name = "test";
                             GenericParameters = [];
                             DeclaringType = None;};},"b");
   AssignmentToReadOnlyLocalField "b"]) ""                                   
        
        testCase "operator is applicable for type" <| fun _ ->
            let semanticCheckResult = 
                @"
                class A
                {
                }
                fun main 
                {
                    var a = new A() + new A();
                }
                "
                |> semanticCheck
            Expect.equal 
                semanticCheckResult 
                (Failure [OperatorNotApplicableForGivenTypes
     (Plus,{Namespace = [];
            Name = "A";
            GenericParameters = [];
            DeclaringType = Some {Namespace = [];
                                  Name = "test";
                                  GenericParameters = [];
                                  DeclaringType = None;};},
      {Namespace = [];
       Name = "A";
       GenericParameters = [];
       DeclaringType = Some {Namespace = [];
                             Name = "test";
                             GenericParameters = [];
                             DeclaringType = None;};})] ) ""
        testCase "entry point" <| fun _ ->
            let semanticCheckResult = 
                @"
                
                "
                |> semanticCheck
            Expect.equal 
                semanticCheckResult 
                (Failure  [NoEntryPointOrMoreThanOne]) ""
        testCase "assignment of invalid type" <| fun _ ->
            let semanticCheckResult = 
                @"
                fun main
                {
                    var a = ""123"";
                    a = 3;
                }
                "
                |> semanticCheck
            Expect.equal 
                semanticCheckResult 
                (Failure [InvalidType ("a",{Namespace = ["System"];
                           Name = "Int32";
                           GenericParameters = [];
                           DeclaringType = None;
                                             },{Namespace = ["System"];
                                                   Name = "String";
                                                   GenericParameters = [];
                                                   DeclaringType = None;
                                             })]) ""
        testCase "duplicate variable declaration" <| fun _ ->
            let semanticCheckResult = 
                @"
                fun main
                {
                    var a = ""123"";
                    var a = 3;
                }
                "
                |> semanticCheck
            Expect.equal 
                semanticCheckResult 
                (Failure [VariableAlreadyDefined "a"]) ""
    ]