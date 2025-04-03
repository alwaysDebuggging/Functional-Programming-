// For more information see https://aka.ms/fsharp-console-apps
open Interpreter
open Interpreter.Parser
<<<<<<< Updated upstream
<<<<<<< Updated upstream
=======
open Interpreter.JParsec.TextParser
>>>>>>> Stashed changes
=======
open Interpreter.JParsec.TextParser
>>>>>>> Stashed changes

let rec parseArgs =
    function
    | []                -> Map.empty
    | name::value::rest -> Map.add name (System.Int32.Parse(value)) (parseArgs rest)
    | _                 -> failwith "Invalid input"


[<EntryPoint>]
let main args =
<<<<<<< Updated upstream
<<<<<<< Updated upstream
    let m = args.[2..] |> Array.toList |> parseArgs
 
    System.IO.File.ReadAllText(args.[1]) |>
    runProgramParser |>
    Result.map
        (fun (prog, body) ->
            Eval.stmntEval body |>
            StateMonad.evalState
                (State.mkState
                     (m |> Map.tryFind "--memSize" |> Option.defaultValue 0)
                     (m |> Map.tryFind "--seed")
                     prog)) |>
    printfn "\n\n%A"
    0
=======
=======
>>>>>>> Stashed changes
    let aa =  run (binop (pchar '+') pint32 pint32 |>>
                 (fun (a, b) -> a + b)) "5 +  7"  |> printfn "%A"

    aa
<<<<<<< Updated upstream
    0
>>>>>>> Stashed changes
=======
    0
>>>>>>> Stashed changes
