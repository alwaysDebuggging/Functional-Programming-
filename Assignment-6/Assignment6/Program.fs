// For more information see https://aka.ms/fsharp-console-apps

open Interpreter.Programs
open Interpreter.Eval
open Interpreter.State
open Interpreter.Memory
open Interpreter.Language

let st = mkState 0 None |> stmntEval (Print ([Num 7; Num 6; Mul (Num 7, Num 6)], "% times % is %, which is the answer to life, the universe, and everything"))

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