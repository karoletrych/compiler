module Compiler.Compiler

open System.Reflection
open System.IO
open CompilerResult
open Parser
open TypeResolving
open TypeInference
open ReferencedAssembliesMetadata
open TypeIdentifiersFinding
open TypeFinding
open SemanticCheck
open IRGeneration

let compile 
    source
    referencedAssemblies =

    let externalTypes = externalTypes referencedAssemblies

    source 
    |> parseModules
    >>= (fun modules -> resolve (modules, typeIdentifiers externalTypes modules))
    >>= (fun modules -> inferTypes (modules, typesDictionary externalTypes modules))
    >>= semanticCheck 
    |> Result.map generateIR
    