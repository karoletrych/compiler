module Compiler.Compiler

open System.Reflection
open System.IO
open Argu
open CompilerResult
open Parser
open TypeFinding
open TypeResolving
open TypeInference
open ReferencedAssembliesMetadata
open Compiler.TypeIdentifiersFinding

type Stage =
    | SyntaxCheck
    | TypeFinding
    | TypeResolving
    | TypeInference
    | SemanticCheck 
    | IRGeneration
    | CILGeneration
    | Full

let compile 
    (sourceFilePaths : string list)
    (outputPath : string)
    (stage : Stage) 
    (compileToDll : bool)=
    let modulesWithPaths = 
        sourceFilePaths 
        |> List.zip (sourceFilePaths |> List.map File.ReadAllText)
    let externalTypes = externalTypes [Assembly.GetAssembly(typeof<obj>)]
    
    match stage with
    | SyntaxCheck -> 
        modulesWithPaths 
        |> parseModules
        |> ignore
    | TypeResolving -> 
        modulesWithPaths 
        |> parseModules
        >>= allKnownTypeIdentifiers externalTypes
        >>= resolve
        |> ignore  
    | TypeInference ->
        modulesWithPaths 
        |> parseModules
        >>= allKnownTypeIdentifiers externalTypes
        >>= resolve
        >>= inferTypes 
        |> ignore  
    | _ -> printfn "Stage not implemented."
    ()
