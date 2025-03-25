module Interpreter.Eval

open Interpreter.Language
open Interpreter.Memory
open Result
open Language
open Interpreter.State


let readFromConsole () = System.Console.ReadLine().Trim()
let tryParseInt (str: string) = System.Int32.TryParse str


let rec readInt () =
    let consoleInput = readFromConsole ()
    let tryParseInput = tryParseInt (consoleInput)
    let (x, y) = tryParseInput

    if x = false then
        printfn "%s is not an integer: " consoleInput
        readInt ()
    else
        y

let rec arithEval (a: aexpr) (st: state) : int option =
    match a with
    | Num int -> Some int

    | Var v -> getVar v st

    | Add(ex1, ex2) ->
        let a = arithEval ex1 st
        let b = arithEval ex2 st

        Option.bind (fun x -> Option.bind (fun y -> Some(x + y)) b) a

    | Mul(ex1, ex2) ->
        let a = arithEval ex1 st
        let b = arithEval ex2 st

        Option.bind (fun x -> Option.bind (fun y -> Some(x * y)) b) a
    | Div(ex1, ex2) ->
        let a = arithEval ex1 st
        let b = arithEval ex2 st

        Option.bind (fun x -> Option.bind (fun y -> if y <> 0 then Some(x / y) else None) b) a
    | Mod(ex1, ex2) ->
        let a = arithEval ex1 st
        let b = arithEval ex2 st

        Option.bind (fun x -> Option.bind (fun y -> if y <> 0 then Some(x % y) else None) b) a
    | MemRead e1 -> Option.bind (fun y -> getMem y st) (arithEval e1 st)

    | Random -> Some(State.random st)

    | Read ->
        let a = readInt ()
        Some(a)
    | Cond(b, a1, a2) ->
        let cond = boolEval b st

        match cond with
        | Some true -> arithEval a1 st

        | Some _ -> arithEval a2 st

        | None -> None

and boolEval b st : bool option =
    match b with
    | TT -> Some true
    | Eq(ex1, ex2) ->
        let a = arithEval ex1 st
        let b = arithEval ex2 st

        Option.bind (fun x -> Option.bind (fun y -> Some((=) x y)) b) a

    | Lt(ex1, ex2) ->
        let a = arithEval ex1 st
        let b = arithEval ex2 st

        Option.bind (fun x -> Option.bind (fun y -> Some((<) x y)) b) a

    | Conj(ex1, ex2) ->
        let a = boolEval ex1 st
        let b = boolEval ex2 st

        Option.bind
            (fun x -> Option.bind (fun y -> Some((&&) x y)) b //(boolEval ex2 st)
            )
            a //(boolEval ex1 st)

    //Some ((&&) a b)

    | Not ex1 ->
        let a = boolEval ex1 st

        a |> Option.bind (fun x -> Some(not x))



let split (s1: string) (s2: string) = s2 |> s1.Split |> Array.toList


let mergeStrings (es: aexpr list) (s: string) (st: state) : string option =
    let rec mergeHelper (es: aexpr list) (s: string list) (acc: string) : string option =
        match es, s with
        | [], [ headS ] -> Some(acc + headS)
        | head_es :: tail_es, headS :: tail_S ->
            match arithEval head_es st with
            | Some y -> mergeHelper tail_es tail_S (acc + headS + (string) y)
            | None -> None
        | _, _ -> None

    mergeHelper es (split s "%") ""

let mergeStrings2 (es: aexpr list) (s: string) (st: state) : string option =
    let rec mergeHelper2 (es: aexpr list) (s: string list) c : string option =
        match es, s with
        | [], [ headS ] -> Some(c headS)
        | head_es :: tail_es, headS :: tail_S ->
            match arithEval head_es st with
            | Some y -> mergeHelper2 tail_es tail_S (fun r -> c (headS + (string) y + r))
            | None -> None
        | _, _ -> None

    mergeHelper2 es (split s "%") id



let rec stmntEval (s: stmnt) (st: state) : state option =
    match s with
    | Skip -> Some st
    | Declare v -> declare v st
    | Assign(v, a) ->
        let b = arithEval a st

        match b with
        | None -> None
        | Some x -> setVar v x st

    | Seq(s1, s2) ->
        let b = stmntEval s1 st

        match b with
        | None -> None
        | Some stt -> stmntEval s2 stt

    | If(gaurd, s1, s2) ->
        let b = boolEval gaurd st

        match b with
        | None -> None
        | Some x -> if x = true then stmntEval s1 st else stmntEval s2 st

    | While(guard, s) ->
        let b = boolEval guard st

        match b with
        | None -> None
        | Some x ->
            if x = true then
                let c = stmntEval s st

                match c with
                | None -> None
                | Some stt -> stmntEval (While(guard, s)) stt
            else
                Some st

    | Alloc(x, e) -> //let size = arithEval e st
        //let dec = getVar x st

        //State.alloc x size st

        Option.bind (fun size -> Option.bind (fun checkDec -> State.alloc x size st) (getVar x st)) (arithEval e st)

    // Option.bind (fun size ->State.alloc x size st
    // ) (arithEval e st)



    | MemWrite(e1, e2) ->
        Option.bind (fun ptr -> Option.bind (fun v -> State.setMem ptr v st) (arithEval e2 st)) (arithEval e1 st)

    //let ptr = arithEval e1 st
    //let v = arithEval e2 st


    | Free(e1, e2) -> (*let ptr = arithEval e1 st
                           let size = arithEval e2 st
                           
                            State.free ptr size st*)

        Option.bind (fun ptr -> Option.bind (fun size -> State.free ptr size st) (arithEval e2 st)) (arithEval e1 st)

    | Print(es, s) ->
        (*let endString = mergeStrings es s st
            match endString with 
            | Some x -> 
                printfn "%A" x
                Some st
            | None -> None*)
        Option.bind
            (fun n ->
                printfn "%A" n
                Some st)
            (mergeStrings es s st)
