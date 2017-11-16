module Compiler.Compiler

open System.Reflection
open System.IO
open CompilerResult
open Parser
open TypeResolving
open TypeInference
open ReferencedAssembliesMetadata
open TypeIdentifiersFinding
open Compiler.ReferencedAssembliesMetadata
open TypeFinding

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
        >>= (fun modules -> resolve (modules, typeIdentifiers externalTypes modules))
        |> ignore  
    | TypeInference ->
        modulesWithPaths 
        |> parseModules
        >>= (fun modules -> resolve (modules, typeIdentifiers externalTypes modules))
        >>= (fun modules -> inferTypes (modules, typesDictionary externalTypes modules))
        |> ignore  
    | _ -> printfn "Stage not implemented."
    ()
