module Interpreter.Program
open Interpreter.Language
open Interpreter.State


let [<EntryPoint>] main _ =
    
    let x = 10 |> mkState |> declare "x" |> Option.bind (alloc "x" 3) |> Option.bind (fun st -> st |> getVar "x" |> Option.bind (fun ptr -> st |> setMem ptr 42 |> Option.bind (free ptr 4)))
    
    
    printfn "%A" x
    0
