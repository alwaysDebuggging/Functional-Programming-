module Interpreter.StateMonad

// Use one of these state monads depending on which combination of green, yellow, and red exercises you are doing.
// Feel free to just delete the rest to unclutter the code.

open State
open Language

// Green exercises

type 'a stateMonad = SM of (state -> ('a * state) option)

let ret x = SM(fun st -> Some(x, st))
let fail = SM(fun _ -> None)

let bind (SM f) g =
    SM(fun st ->
        match f st with
        | Some(x, st') -> let (SM h) = g x in h st'
        | None -> None)

let (>>=) a f = bind a f
let (>>>=) a b = a >>= (fun _ -> b)

let declare (str: string) : unit stateMonad =
    SM(fun st ->

        Option.bind (fun x -> Some((), x)) (State.declare str st)

    )

// ret () >>=   SM (fun x -> Some ((), (x)) (State.declare str st))

let setVar (str: string) (v: int) : unit stateMonad =
    SM(fun st -> Option.bind (fun x -> Some((), x)) (State.setVar str v st))

let getVar (str: string) : int stateMonad =
    SM(fun st -> Option.bind (fun x -> Some(x, st)) (State.getVar str st))

let alloc (str: string) (size: int) : unit stateMonad =
    SM(fun st -> Option.bind (fun x -> Some((), x)) (State.alloc str size st))

let free (ptr: int) (size: int) : unit stateMonad =
    SM(fun st -> Option.bind (fun x -> Some((), x)) (State.free ptr size st))

let setMem (ptr: int) (v: int) : unit stateMonad =
    SM(fun st -> Option.bind (fun x -> Some((), x)) (State.setMem ptr v st))

let getMem (ptr: int) : int stateMonad =
    SM(fun st -> Option.bind (fun x -> Some(x, st)) (State.getMem ptr st))

let random: int stateMonad = SM(fun st -> Some(State.random st, st))

let evalState (SM f) (st: state) : 'a option =
    match f st with
    | Some(v, st) -> Some(v)
    | None -> None
