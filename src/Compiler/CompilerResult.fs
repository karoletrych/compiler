module Compiler.CompilerResult
open Compiler.Ast


type Failure = 
| SyntaxError of string
| TypeNotFound of TypeSpec
| LocalFunctionNotFound of string * TypeIdentifier list * TypeIdentifier list
| FunctionNotFound of TypeIdentifier * string * TypeIdentifier list * TypeSpec list
| FieldNotFound of TypeIdentifier * string
| CannotInferType of string
| UndefinedVariable of string

type CompilerResult<'TSuccess> = 
| Success of 'TSuccess
| Failure of Failure list
with member x.Value = 
        match x with
        | Success x -> x
        | Failure errors -> failwith "Value retrieved from CompilerResult being Failure %A" errors

module Result = 
    let succeed x = Success x 
    let failure error = Failure [error]
    let succeedUnit = Success ((), []) 

    let get = function
        | Success x -> x
        | Failure errors -> failwith (sprintf "get called on CompilerResult being Failure %A" errors)
    let getErrors = function
        | Success x -> failwith "getErrors called on CompilerResult being Success"
        | Failure errors -> errors
   
    let isFailure = function
        | Success _ -> false
        | Failure _ -> true 
    let isSuccess = function
        | Success _ -> true
        | Failure _ -> false 

    let either fSuccess fFailure = function
        | Success x -> fSuccess x 
        | Failure errors -> fFailure errors 

    let mergeErrors errors result =
        let fSuccess x = 
            Success x
        let fFailure failureErrors = 
            Failure (failureErrors @ errors) 
        either fSuccess fFailure result

    let bind f result =
        let fSuccess x = 
            f x 
        let fFailure errs = 
            Failure errs 
        result |> either fSuccess fFailure 
    let bind2 f r1 r2 =
        match (r1, r2) with
        | Success s1, Success s2 -> f s1 s2
        | Failure f1, Success _  -> Failure f1
        | Success _ , Failure f2 -> Failure f2
        | Failure f1, Failure f2 -> Failure (f1 @ f2)

    let map f result =
        let fSuccess x = 
            f x |> Success
        let fFailure errs = 
            Failure errs 
        result |> either fSuccess fFailure 

    let map2 op r1 r2 = 
         match (r1, r2) with
         | Success s1, Success s2 -> Success (op s1 s2)
         | Failure f1, Success _  -> Failure f1
         | Success _ , Failure f2 -> Failure f2
         | Failure f1, Failure f2 -> Failure (f1 @ f2)

    let merge (results : CompilerResult<'a> list) = 
        match results with
        | [] -> succeed []
        | list -> if list |> List.forall isSuccess
                  then list |> List.map get |> succeed
                  else list 
                       |> List.where isFailure
                       |> List.collect getErrors
                       |> Failure 
    let mapOption foo o = 
        match o with
        | Some x -> x |> foo |> bind (Some >> Success)
        | None -> succeed None
    let orElse value r = 
        match r with
        | Success s -> Success s
        | Failure _ -> Success value

    let apply f x = map2 (fun f x -> f x) f x

    let inline (<!>) f x = map f x
    let inline (<*>) f x = apply f x
    let map3 f x y z = f <!> x <*> y <*> z

let inline (<!>) f x = Result.map f x
let inline (<*>) f x = Result.apply f x
let inline (&&=) v1 v2 = 
    Result.map2 v1 v2
let inline (>>=) twoTrackInput switchFunction =
    Result.bind switchFunction twoTrackInput
