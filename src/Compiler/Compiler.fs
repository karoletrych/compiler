module Compiler.Compiler

open CompilerResult

let compile 
    source
    referencedAssemblies 
    isExe =

    let externalTypes = ReferencedDllsMetadataRetriever.getExternalTypes referencedAssemblies
    source 
    |> Parser.parseModules
    >>= (fun modules -> TypeResolving.resolve (modules, TypeIdentifiersFinding.find externalTypes modules))
    >>= (fun modules -> TypeInference.inferTypes (modules, TypeFinding.find externalTypes modules))
    >>= SemanticCheck.check isExe
    |> Result.map IRGeneration.generateIR
    