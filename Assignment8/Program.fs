// For more information see https://aka.ms/fsharp-console-apps
open Interpreter
open Interpreter.Parser
open Interpreter.FParsecLight.TextParser


let rec parseArgs =
    function
    | []                -> Map.empty
    | name::value::rest -> Map.add name (System.Int32.Parse(value)) (parseArgs rest)
    | _                 -> failwith "Invalid input"


[<EntryPoint>]
let main args =
    let aa =  run pbexpr "true" |> printfn "%A"

    printfn "%A" aa
    0
