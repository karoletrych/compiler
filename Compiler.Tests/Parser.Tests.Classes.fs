module Compiler.Parser.Tests.Classes

open Expecto
open Compiler.Ast
open Compiler.Parser

[<Tests>]
let tests =
  testList "Parser.Tests.Classes" [
    testCase "empty class" <| fun _ ->
      let source = "
        class A
        {
        }
        "
      Expect.equal (parse source) 
          [ClassDeclaration {Type = (Identifier "A");
                   GenericTypeParameters = [];
                   BaseTypes = [];
                   ValueDeclarations = [];
                   FieldDeclarations = [];
                   Constructor = None;
                   FunctionDeclarations = [];}] ""
    testCase "class with fields" <| fun _ ->
      let source = "
        class A
        {
            val a : A = new A()
            var i : int = 3
            var f : float = 3
        }
        "
      Expect.equal (parse source) 
        [ClassDeclaration
           {Type = (Identifier "A");
            GenericTypeParameters = [];
            BaseTypes = [];
            ValueDeclarations =
             [(Identifier "a",
               Some
                 (CustomTypeSpec
                    ([], CustomType((Identifier "A"),[]))),
               (Some (NewExpression (CustomTypeSpec([], CustomType((Identifier "A"),[])),[]))))];
            FieldDeclarations =
             [FullDeclaration (Identifier "i",Int,LiteralExpression (IntLiteral 3));
              FullDeclaration (Identifier "f",Float,LiteralExpression (IntLiteral 3))];
            Constructor = None;
            FunctionDeclarations = [];}] ""
    testCase "class with functions" <| fun _ ->
      let source = "
        class A
        {
            var i : int

            fun increment
            {
                i = i + 1;
            }

            fun getI
            {
                return i;
            }
        }
        class B {
            fun useClassA
            {
                val a = new A();
                a.increment();
                a.increment();
                a.increment();
                if a.getI() == 3
                    return true;
                return false;
            }
        }
        "
      Expect.equal (parse source) 
          [ClassDeclaration
       {Type = (Identifier "A");
        GenericTypeParameters = [];
        BaseTypes = [];
        ValueDeclarations = [];
        FieldDeclarations = [DeclarationWithType (Identifier "i",Int)];
        Constructor = None;
        FunctionDeclarations =
         [{Name = Identifier "increment";
          GenericParameters = [];
          Parameters =  [];
          ReturnType = None;
          Body = 
           [AssignmentStatement
              (IdentifierExpression (Identifier "i"),
               BinaryExpression
                 (IdentifierExpression (Identifier "i"),Add,
                  LiteralExpression (IntLiteral 1)))]};
          {Name=Identifier "getI";
GenericParameters=[];
Parameters= [];
ReturnType= None;
Body=
           [ReturnStatement (Some (IdentifierExpression (Identifier "i")))]}];};
     ClassDeclaration
         {Type = (Identifier "B");
          GenericTypeParameters = [];
          BaseTypes = [];
          ValueDeclarations = [];
          FieldDeclarations = [];
          Constructor = None;
          FunctionDeclarations =
           [{Name = Identifier "useClassA"; GenericParameters = []; Parameters = []; ReturnType = None;
             Body = 
             [ValueDeclaration
                (Identifier "a", None,
                 NewExpression
                   (CustomTypeSpec([],CustomType ((Identifier "A"),[])),[]));
              MemberFunctionCallStatement
                (MemberFunctionCall
                   (IdentifierExpression (Identifier "a"),
                    FunctionCallExpression (FunctionCall (Identifier "increment", [], []))));
              MemberFunctionCallStatement
                (MemberFunctionCall
                   (IdentifierExpression (Identifier "a"),
                    FunctionCallExpression (FunctionCall (Identifier "increment", [], []))));
              MemberFunctionCallStatement
                (MemberFunctionCall
                   (IdentifierExpression (Identifier "a"),
                    FunctionCallExpression (FunctionCall (Identifier "increment", [], []))));
              IfStatement
                (BinaryExpression
                   (MemberExpression
                      (MemberFunctionCall
                         (IdentifierExpression (Identifier "a"),
                          FunctionCallExpression (FunctionCall (Identifier "getI", [], [])))),Equal,
                    LiteralExpression (IntLiteral 3)),
                 ReturnStatement (Some (LiteralExpression (BoolLiteral true))),None);
              ReturnStatement (Some (LiteralExpression (BoolLiteral false)))]}];}]""
    
    testCase "generic class" <| fun _ ->
      let source = "class A<T>{}"
      Expect.equal (parse source) 
          [ClassDeclaration
         {Type = (Identifier "A");
          GenericTypeParameters = [GenericTypeParameter (Identifier "T")];
          BaseTypes = [];
          ValueDeclarations = [];
          FieldDeclarations = [];
          Constructor = None;
          FunctionDeclarations = [];}] "" 
    testCase "generic class with 2 type parameters" <| fun _ ->
      let source = "class A<T, V>{}"
      Expect.equal (parse source) 
          [ClassDeclaration
         {Type = (Identifier "A");
          GenericTypeParameters = [GenericTypeParameter (Identifier "T"); GenericTypeParameter (Identifier "V")];
          BaseTypes = [];
          ValueDeclarations = [];
          FieldDeclarations = [];
          Constructor = None;
          FunctionDeclarations = [];}] ""          
    testCase "generic class with inheritance and base constructor call" <| fun _ ->
      let source = "class B<T, V>
    {
        val _tValue : T
        val _vValue : V

        construct (tValue : T) (vValue : V)
        {
            _tValue = tValue;
            _vValue = vValue;
        }
    }
    class A<T> : B<T, int>
    {
        val _tValue : T
        val _number : int

        construct (tValue : T) (number : int) : (tValue, number)
        {
            _tValue = tValue;
            _number = number;
        }
    }"
      Expect.equal (parse source) 
           [ClassDeclaration
         {Type = (Identifier "B");
          GenericTypeParameters =
           [GenericTypeParameter (Identifier "T");
            GenericTypeParameter (Identifier "V")];
          BaseTypes = [];
          ValueDeclarations =
           [(Identifier "_tValue",
             Some
               (CustomTypeSpec
                  ([],CustomType ((Identifier "T"),[]))),
             None);
            (Identifier "_vValue",
             Some
               (CustomTypeSpec
                  ([],CustomType ((Identifier "V"),[]))),
             None)];
          FieldDeclarations = [];
          Constructor =
           Some
             {Parameters =
               [(Identifier "tValue",
                 CustomTypeSpec
                   ([],CustomType ((Identifier "T"),[])));
                (Identifier "vValue",
                 CustomTypeSpec
                   ([],CustomType ((Identifier "V"),[])))];
              BaseClassConstructorCall = [];
              Statements =
               [AssignmentStatement
                  (IdentifierExpression (Identifier "_tValue"),
                   IdentifierExpression (Identifier "tValue"));
                AssignmentStatement
                  (IdentifierExpression (Identifier "_vValue"),
                   IdentifierExpression (Identifier "vValue"))];};
          FunctionDeclarations = [];};
       ClassDeclaration
         {Type = (Identifier "A");
          GenericTypeParameters = [GenericTypeParameter (Identifier "T")];
          BaseTypes =
           [([],CustomType
                 ((Identifier "B"),
                  [CustomTypeSpec
                     ([],CustomType ((Identifier "T"),[]));
                   Int]))];
          ValueDeclarations =
           [(Identifier "_tValue",
             Some
               (CustomTypeSpec
                  ([],CustomType ((Identifier "T"),[]))),
             None); (Identifier "_number", Some Int, None)];
          FieldDeclarations = [];
          Constructor =
           Some
             {Parameters =
               [(Identifier "tValue",
                 CustomTypeSpec
                   ([],CustomType ((Identifier "T"),[])));
                (Identifier "number", Int)];
              BaseClassConstructorCall =
               [IdentifierExpression (Identifier "tValue");
                IdentifierExpression (Identifier "number")];
              Statements =
               [AssignmentStatement
                  (IdentifierExpression (Identifier "_tValue"),
                   IdentifierExpression (Identifier "tValue"));
                AssignmentStatement
                  (IdentifierExpression (Identifier "_number"),
                   IdentifierExpression (Identifier "number"))];};
          FunctionDeclarations = [];}] ""                  
    testCase "generic function declaration" <| fun _ ->
      let source = "
          fun add<T,V,U> (t : T) (v : V) (u : U)
          {
            return t+v+u;
          }
        "
      Expect.equal (parse source) 
          [FunctionDeclaration
       {Name = Identifier "add";
        GenericParameters = [GenericTypeParameter (Identifier "T");
         GenericTypeParameter (Identifier "V");
         GenericTypeParameter (Identifier "U")];
        Parameters = [(Identifier "t",
                          CustomTypeSpec
                            ([],CustomType ((Identifier "T"),[])));
                       (Identifier "v",
                        CustomTypeSpec
                          ([],CustomType ((Identifier "V"),[])));
                       (Identifier "u",
                        CustomTypeSpec
                          ([],CustomType ((Identifier "U"),[])))]; ReturnType =  None;
                      Body = [ReturnStatement
                         (Some
                            (BinaryExpression
                               (BinaryExpression
                                  (IdentifierExpression (Identifier "t"),Add,
                                   IdentifierExpression (Identifier "v")),Add,
                                IdentifierExpression (Identifier "u"))))]}] 
                    ""
    ]




open System.Text.RegularExpressions
let regex = Regex(Regex.Escape(","));
let names = ["Name" ;
  "GenericParameters" ;
  "Parameters" ;
  "ReturnType" ;
  "FunctionBody" ]
@"(Identifier ""getI"",[], [], None, [ReturnStatement (Some (IdentifierExpression (Identifier ""i"")))])"
|> fun x -> "{" + x.Substring(1, (x.Length-2)) + "}"
|> fun x -> x.Insert(1, names.[0] + "=")
|> fun x -> names |> List.fold (fun state str -> regex.Replace(state, ";" + str + "=", 1)) x
|> printfn "%s"

