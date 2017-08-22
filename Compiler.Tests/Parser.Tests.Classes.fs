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
          [ClassDeclaration {Type = NonGenericTypeSpec (Identifier "A");
                   GenericTypeParameters = [];
                   BaseTypes = [];
                   ValueDeclarations = [];
                   FieldsDeclarations = [];
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
           {Type = NonGenericTypeSpec (Identifier "A");
            GenericTypeParameters = [];
            BaseTypes = [];
            ValueDeclarations =
             [(Identifier "a",
               Some
                 (CustomType
                    (NonGenericCustomTypeSpec (NonGenericTypeSpec (Identifier "A")))),
               (Some (NewExpression (NonGenericCustomTypeSpec (NonGenericTypeSpec (Identifier "A")),[]))))];
            FieldsDeclarations =
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
       {Type = NonGenericTypeSpec (Identifier "A");
        GenericTypeParameters = [];
        BaseTypes = [];
        ValueDeclarations = [];
        FieldsDeclarations = [DeclarationWithType (Identifier "i",Int)];
        Constructor = None;
        FunctionDeclarations =
         [(Identifier "increment",[], [], None,
           [AssignmentStatement
              (IdentifierExpression (Identifier "i"),
               BinaryExpression
                 (IdentifierExpression (Identifier "i"),Add,
                  LiteralExpression (IntLiteral 1)))]);
          (Identifier "getI",[], [], None,
           [ReturnStatement (Some (IdentifierExpression (Identifier "i")))])];};
     ClassDeclaration
         {Type = NonGenericTypeSpec (Identifier "B");
          GenericTypeParameters = [];
          BaseTypes = [];
          ValueDeclarations = [];
          FieldsDeclarations = [];
          Constructor = None;
          FunctionDeclarations =
           [(Identifier "useClassA",[], [], None,
             [ValueDeclaration
                (Identifier "a", None,
                 NewExpression
                   (NonGenericCustomTypeSpec (NonGenericTypeSpec (Identifier "A")),[]));
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
              ReturnStatement (Some (LiteralExpression (BoolLiteral false)))])];}]""
    
    testCase "generic class" <| fun _ ->
      let source = "class A<T>{}"
      Expect.equal (parse source) 
          [ClassDeclaration
         {Type = NonGenericTypeSpec (Identifier "A");
          GenericTypeParameters = [GenericTypeParameter (Identifier "T")];
          BaseTypes = [];
          ValueDeclarations = [];
          FieldsDeclarations = [];
          Constructor = None;
          FunctionDeclarations = [];}] "" 
    testCase "generic class with 2 type parameters" <| fun _ ->
      let source = "class A<T, V>{}"
      Expect.equal (parse source) 
          [ClassDeclaration
         {Type = NonGenericTypeSpec (Identifier "A");
          GenericTypeParameters = [GenericTypeParameter (Identifier "T"); GenericTypeParameter (Identifier "V")];
          BaseTypes = [];
          ValueDeclarations = [];
          FieldsDeclarations = [];
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
         {Type = NonGenericTypeSpec (Identifier "B");
          GenericTypeParameters =
           [GenericTypeParameter (Identifier "T");
            GenericTypeParameter (Identifier "V")];
          BaseTypes = [];
          ValueDeclarations =
           [(Identifier "_tValue",
             Some
               (CustomType
                  (NonGenericCustomTypeSpec (NonGenericTypeSpec (Identifier "T")))),
             None);
            (Identifier "_vValue",
             Some
               (CustomType
                  (NonGenericCustomTypeSpec (NonGenericTypeSpec (Identifier "V")))),
             None)];
          FieldsDeclarations = [];
          Constructor =
           Some
             {Parameters =
               [(Identifier "tValue",
                 CustomType
                   (NonGenericCustomTypeSpec (NonGenericTypeSpec (Identifier "T"))));
                (Identifier "vValue",
                 CustomType
                   (NonGenericCustomTypeSpec (NonGenericTypeSpec (Identifier "V"))))];
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
         {Type = NonGenericTypeSpec (Identifier "A");
          GenericTypeParameters = [GenericTypeParameter (Identifier "T")];
          BaseTypes =
           [CustomType
              (GenericCustomTypeSpec
                 (NonGenericTypeSpec (Identifier "B"),
                  [CustomType
                     (NonGenericCustomTypeSpec (NonGenericTypeSpec (Identifier "T")));
                   Int]))];
          ValueDeclarations =
           [(Identifier "_tValue",
             Some
               (CustomType
                  (NonGenericCustomTypeSpec (NonGenericTypeSpec (Identifier "T")))),
             None); (Identifier "_number", Some Int, None)];
          FieldsDeclarations = [];
          Constructor =
           Some
             {Parameters =
               [(Identifier "tValue",
                 CustomType
                   (NonGenericCustomTypeSpec (NonGenericTypeSpec (Identifier "T"))));
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
       (Identifier "add",
        [GenericTypeParameter (Identifier "T");
         GenericTypeParameter (Identifier "V");
         GenericTypeParameter (Identifier "U")],
        [(Identifier "t",
          CustomType
            (NonGenericCustomTypeSpec (NonGenericTypeSpec (Identifier "T"))));
         (Identifier "v",
          CustomType
            (NonGenericCustomTypeSpec (NonGenericTypeSpec (Identifier "V"))));
         (Identifier "u",
          CustomType
            (NonGenericCustomTypeSpec (NonGenericTypeSpec (Identifier "U"))))], None,
        [ReturnStatement
           (Some
              (BinaryExpression
                 (BinaryExpression
                    (IdentifierExpression (Identifier "t"),Add,
                     IdentifierExpression (Identifier "v")),Add,
                  IdentifierExpression (Identifier "u"))))])] 
                  ""


  ]