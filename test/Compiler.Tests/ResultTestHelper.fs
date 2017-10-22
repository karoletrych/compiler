module Compiler.Tests.ResultTestHelper
open Expecto
open Compiler.Result

/// Expects the value to be a Result.Ok value.
let isOk x message =
  match x with
  | Success _ -> ()
  | Failure x ->
    failtestf "%s. Expected Ok, was Error(%A)." message x

/// Expects the value to be a Result.Error value.
let isError x message =
  match x with
  | Success _ ->
    failtestf "%s. Expected Error _, was Ok(%A)." message x
  | Failure _ -> ()