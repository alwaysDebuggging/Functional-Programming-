// For more information see https://aka.ms/fsharp-console-apps
open Interpreter
open Interpreter.Parser

open Interpreter.JParsec.TextParser

open Interpreter.JParsec.TextParser


let rec parseArgs =
    function
    | []                -> Map.empty
    | name::value::rest -> Map.add name (System.Int32.Parse(value)) (parseArgs rest)
    | _                 -> failwith "Invalid input"


[<EntryPoint>]
let main args =
    let aa =  run (binop (pchar '+') pint32 pint32 |>>
                 (fun (a, b) -> a + b)) "5 +  7"  |> printfn "%A"

    aa

    0
