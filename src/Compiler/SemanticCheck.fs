#if !(INTERACTIVE)
module Compiler.SemanticCheck
#endif

#if INTERACTIVE
#load "Ast.fs"
#load "Types.fs"
#load "Result.fs"
#endif

open Compiler.Types
open Compiler.Ast
open Compiler.TypeFinding
open Compiler.CompilerResult
open Compiler.Identifier
open System



// let scanModule modul =
    // TypeChecker.scanAst (TypeChecker.scanType (allKnownTypes (fst modul) (snd modul)))

