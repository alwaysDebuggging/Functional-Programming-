// For more information see https://aka.ms/fsharp-console-apps
open Interpreter
open Interpreter.Parser
open Interpreter.FParsecLight.TextParser


let rec parseArgs =
    function
    | [] -> Map.empty
    | name :: value :: rest -> Map.add name (System.Int32.Parse(value)) (parseArgs rest)
    | _ -> failwith "Invalid input"


[<EntryPoint>]
let main args =
    let aa = run pstmnt "if (x < y) { x := 5; y\n := 27 / (5 %\t 0) } else { y := 7 }"


    printfn "%A" aa

    0
