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
               NewExpression
                 (NonGenericCustomTypeSpec (NonGenericTypeSpec (Identifier "A")),[]))];
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
         [(Identifier "increment", [], None,
           [AssignmentStatement
              (IdentifierExpression (Identifier "i"),
               BinaryExpression
                 (IdentifierExpression (Identifier "i"),Add,
                  LiteralExpression (IntLiteral 1)))]);
          (Identifier "getI", [], None,
           [ReturnStatement (Some (IdentifierExpression (Identifier "i")))])];};
     ClassDeclaration
       {Type = NonGenericTypeSpec (Identifier "B");
        GenericTypeParameters = [];
        BaseTypes = [];
        ValueDeclarations = [];
        FieldsDeclarations = [];
        Constructor = None;
        FunctionDeclarations =
         [(Identifier "useClassA", [], None,
           [ValueDeclaration
              (Identifier "a", None,
               NewExpression
                 (NonGenericCustomTypeSpec (NonGenericTypeSpec (Identifier "A")),[]));
            MemberFunctionCallStatement
              (MemberFunctionCall
                 (IdentifierExpression (Identifier "a"),
                  FunctionCallExpression (Identifier "increment", [])));
            MemberFunctionCallStatement
              (MemberFunctionCall
                 (IdentifierExpression (Identifier "a"),
                  FunctionCallExpression (Identifier "increment", [])));
            MemberFunctionCallStatement
              (MemberFunctionCall
                 (IdentifierExpression (Identifier "a"),
                  FunctionCallExpression (Identifier "increment", [])));
            IfStatement
              (BinaryExpression
                 (MemberExpression
                    (MemberFunctionCall
                       (IdentifierExpression (Identifier "a"),
                        FunctionCallExpression (Identifier "getI", []))),Equal,
                  LiteralExpression (IntLiteral 3)),
               ReturnStatement (Some (IdentifierExpression (Identifier "true"))),
               None);
            ReturnStatement (Some (IdentifierExpression (Identifier "false")))])];}] ""
   ]
  
