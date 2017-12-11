module Compiler.SemanticCheck

open Compiler.CompilerResult

// Czy typy value są przypisywane w konstruktorze.
// Check whether if and while exoressions are of type Bool
// Value declaration i variableDeclaration.FullDeclaration type matches expressionType
// Declarations: variable was not defined
// Operator jest aplikowalny dla typu
// Value is not being assigned after initialization
// Variable already defined
// Identifier not defined
// Invalid argument
// Wrong number of arguments
// Break -> no enclosing loop
// Function already defined.
// No entry point (main)
// Sprawdzanie cyklicznego dziedziczenia
// Nazwy zmiennych w ramach funkcji (nie scopea) są unikalne
// argumenty dla operatorów są poprawne
// if(p == 0) return 0; else if(p == 1) return "1"; else if(p == 2) return 2.0;
// sprawdzenie wywolan konstruktorow klas bazowych

let semanticCheck modules =
    Result.succeed modules
