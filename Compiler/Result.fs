module Compiler.Result

//TODO:
type Location = {
    Module : string;
    Line : uint32;
    Column : uint32;
}

type Failure<'T> = 
| CannotResolveType of 'T

type Result<'TSuccess, 'TError> = 
| Success of 'TSuccess * Failure<'TError> list
| Failure of Failure<'TError> list

let succeed x = Success (x, []) 
let succeedUnit = Success ((), []) 

let either fSuccess fFailure = function
    | Success (x,errors) -> fSuccess (x,errors) 
    | Failure errors -> fFailure errors 

let mergeErrors errors result =
    let fSuccess (x,successErrors) = 
        Success (x, errors @ successErrors) 
    let fFailure failureErrors = 
        Failure (failureErrors @ errors) 
    either fSuccess fFailure result

let bind f result =
    let fSuccess (x,errors) = 
        f x |> mergeErrors errors
    let fFailure errs = 
        Failure errs 
    result |> either fSuccess fFailure 


let (>>=) twoTrackInput switchFunction =
    bind switchFunction twoTrackInput