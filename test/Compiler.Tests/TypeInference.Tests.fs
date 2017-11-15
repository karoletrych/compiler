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
  let infer program =
    let externals = externalTypes [System.Reflection.Assembly.GetAssembly(typeof<obj>)]
    [("test", program)]
    |> parseModules 
    >>= allKnownTypeIdentifiers externals
    >>= resolve
    >>= allKnownTypes externals
    >>= inferTypes
    |> Result.map (fun m -> m.Classes)



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
         (Result.failure (CannotInferType
     "Cannot infer type of binary expression of types: SystemInt32 and SystemSingle")) ""
    testCase "recursive type inference fails" <| fun _ ->
        let inference = infer "
                fun factorial (n : int) : int
                {
                    if n==0
                        return 1;
                    else
                        return n * factorial(n-1);
                }"
        Expect.equal inference (Result.succeed [{Name = "test";
        GenericTypeParameters = [];
        BaseClass = None;
        ImplementedInterfaces = [];
        Properties = [];
        Constructor = None;
        FunctionDeclarations =
         [{Name = "factorial";
           GenericParameters = [];
           Parameters =
            [("n", TypeIdentifier {Namespace = ["System"];
                                   TypeName = {Name = ["Int32"];
                                               GenericArguments = [];};})];
           ReturnType =
            Some (TypeIdentifier {Namespace = ["System"];
                                  TypeName = {Name = ["Int32"];
                                              GenericArguments = [];};});
           Body =
            [IfStatement
               (InferredTypeExpression
                  (BinaryExpression
                     (IdentifierExpression "n",Equal,
                      LiteralExpression (IntLiteral 0)),
                   {Namespace = ["System"];
                    TypeName = {Name = ["Boolean"];
                                GenericArguments = [];};}),
                ReturnStatement
                  (Some
                     (InferredTypeExpression
                        (LiteralExpression (IntLiteral 1),
                         {Namespace = ["System"];
                          TypeName = {Name = ["Int32"];
                                      GenericArguments = [];};}))),
                Some
                  (ReturnStatement
                     (Some
                        (InferredTypeExpression
                           (BinaryExpression
                              (IdentifierExpression "n",Multiplication,
                               FunctionCallExpression
                                 {Name = "factorial";
                                  GenericArguments = [];
                                  Arguments =
                                   [BinaryExpression
                                      (IdentifierExpression "n",Minus,
                                       LiteralExpression (IntLiteral 1))];}),
                            {Namespace = ["System"];
                             TypeName = {Name = ["Int32"];
                                         GenericArguments = [];};})))))];}];}]) ""
    testCase "basic types" <| fun _ ->
        let inference = infer @"
                fun main 
                {
                    val name = ""Karol"";
                    val age = 22;
                    val weight = 65.1;
                    val arr = [1;2;3; ""string""; name; age; weight];
                }"
        Expect.equal inference (Result.succeed [{Name = "test";
    GenericTypeParameters = [];
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
            InferredTypeExpression
              (LiteralExpression (StringLiteral "Karol"),
               {Namespace = ["System"];
                TypeName = {Name = ["String"];
                            GenericArguments = [];};}));
         ValueDeclaration
           ("age", None,
            InferredTypeExpression
              (LiteralExpression (IntLiteral 22),
               {Namespace = ["System"];
                TypeName = {Name = ["Int32"];
                            GenericArguments = [];};}));
         ValueDeclaration
           ("weight", None,
            InferredTypeExpression
              (LiteralExpression (FloatLiteral 65.1),
               {Namespace = ["System"];
                TypeName = {Name = ["Single"];
                            GenericArguments = [];};}));
         ValueDeclaration
           ("arr", None,
            InferredTypeExpression
              (ListInitializerExpression
                 [LiteralExpression (IntLiteral 1);
                  LiteralExpression (IntLiteral 2);
                  LiteralExpression (IntLiteral 3);
                  LiteralExpression (StringLiteral "string");
                  IdentifierExpression "name"; IdentifierExpression "age";
                  IdentifierExpression "weight"],
               {Namespace = ["Generic"; "Collections"; "System"];
                TypeName =
                 {Name = ["List"];
                  GenericArguments = [{Namespace = ["System"];
                                       TypeName = {Name = ["Object"];
                                                   GenericArguments = [];};}];};}))];}];}])  ""
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
        Expect.equal inference (Result.succeed [{Name = "test";
    GenericTypeParameters = [];
    BaseClass = None;
    ImplementedInterfaces = [];
    Properties = [];
    Constructor = None;
    FunctionDeclarations =
     [{Name = "testFunction";
       GenericParameters = [];
       Parameters = [];
       ReturnType = None;
       Body =
        [ValueDeclaration
           ("types", None,
            InferredTypeExpression
              (ListInitializerExpression
                 [NewExpression
                    (TypeIdentifier {Namespace = ["System"];
                                     TypeName = {Name = ["Object"];
                                                 GenericArguments = [];};},[]);
                  NewExpression
                    (TypeIdentifier {Namespace = [];
                                     TypeName = {Name = ["A"; "test"];
                                                 GenericArguments = [];};},[]);
                  NewExpression
                    (TypeIdentifier {Namespace = [];
                                     TypeName = {Name = ["B"; "test"];
                                                 GenericArguments = [];};},[]);
                  NewExpression
                    (TypeIdentifier {Namespace = [];
                                     TypeName = {Name = ["C"; "test"];
                                                 GenericArguments = [];};},[]);
                  LiteralExpression (StringLiteral "str");
                  LiteralExpression (IntLiteral 42)],
               {Namespace = ["Generic"; "Collections"; "System"];
                TypeName =
                 {Name = ["List"];
                  GenericArguments = [{Namespace = ["System"];
                                       TypeName = {Name = ["Object"];
                                                   GenericArguments = [];};}];};}))];}];};
   {Name = "test+A";
    GenericTypeParameters = [];
    BaseClass = None;
    ImplementedInterfaces = [];
    Properties = [];
    Constructor = None;
    FunctionDeclarations = [];};
   {Name = "test+B";
    GenericTypeParameters = [];
    BaseClass = Some (TypeIdentifier {Namespace = [];
                                      TypeName = {Name = ["A"; "test"];
                                                  GenericArguments = [];};});
    ImplementedInterfaces = [];
    Properties = [];
    Constructor = None;
    FunctionDeclarations = [];};
   {Name = "test+C";
    GenericTypeParameters = [];
    BaseClass = Some (TypeIdentifier {Namespace = [];
                                      TypeName = {Name = ["A"; "test"];
                                                  GenericArguments = [];};});
    ImplementedInterfaces = [];
    Properties = [];
    Constructor = None;
    FunctionDeclarations = [];}]) ""
]