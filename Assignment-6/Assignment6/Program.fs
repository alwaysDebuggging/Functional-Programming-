// For more information see https://aka.ms/fsharp-console-apps

open Interpreter.Programs
open Interpreter.Eval
open Interpreter.State
open Interpreter.Memory
open Interpreter.Language

let st = 42 |> Some |> mkState 0

printf "%A" st

// let runProgram prog =
//     42 |>
//     Some |>
//     mkState 10 |>
//     stmntEval prog |>
//     ignore

// Uncomment the program you want to run

// runProgram guessANumber
//runProgram bubbleSort