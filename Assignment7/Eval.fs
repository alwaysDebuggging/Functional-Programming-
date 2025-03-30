module Interpreter.Eval


open Interpreter.Memory
open Result
open Language
open StateMonad


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

type StateBuilder() =  
    member this.Bind(f, x) = (>>=) f x  
    member this.Return(x) = ret x  
    member this.ReturnFrom(x) = x  
    member this.Combine(a, b) = a >>= (fun _ -> b) 
      
let eval = StateBuilder()


let rec arithEval (a: aexpr) : int stateMonad =
    match a with
    | Num v -> ret v

    | Var v -> getVar v >>= fun a->
        ret a

    | Add(ex1, ex2) ->
        arithEval ex1 >>= fun a ->
        arithEval ex2 >>= fun b ->

        ret ((+) a b)

    | Mul(ex1, ex2) ->
        arithEval ex1 >>= fun a ->
        arithEval ex2 >>= fun b ->

        ret (a * b)

    | Div(ex1, ex2) ->
        arithEval ex1 >>= fun a ->
        arithEval ex2 >>= fun b ->

       if b <> 0  then ret ((/) a b) else fail 

    | Mod(ex1, ex2) ->
        arithEval ex1 >>= fun a ->
        arithEval ex2 >>= fun b ->

        if b <> 0  then ret ((%) a b) else fail

    | MemRead e1 -> 
        arithEval e1 >>= fun a ->
        getMem a >>= fun b ->

        ret b

    | Random -> 
        random >>= fun a ->

        ret a

    | Read ->
        let a = readInt ()
        ret a

    | Cond(b, a1, a2) -> 
        boolEval b >>= fun bl ->
            if bl then
                arithEval a1 >>= fun x -> ret x
            else 
                arithEval a2 >>= fun x -> ret x

and boolEval (b ) : bool stateMonad =
    match b with
    | TT -> ret true
    | Eq(ex1, ex2) ->
        arithEval ex1  >>= fun x ->
        arithEval ex2  >>= fun y ->

        ret ((=) x y)

    | Lt(ex1, ex2) ->
        arithEval ex1 >>= fun x ->
        arithEval ex2 >>= fun y ->

        ret ((<) x y)

    | Conj(ex1, ex2) ->
        boolEval ex1 >>= fun x ->
        boolEval ex2 >>= fun y ->

        ret ((&&) x  y)

    | Not ex1 -> boolEval ex1 >>= fun x -> 
        ret (not x)


let split (s1: string) (s2: string) = s2 |> s1.Split |> Array.toList


let mergeStrings (es: aexpr list) (s: string) : string stateMonad =
    let rec mergeHelper (es: aexpr list) (s: string list) (acc: string) : string stateMonad =
        match es, s with
        | [], [ headS ] -> ret (acc + headS)
        | head_es :: tail_es, headS :: tail_S ->
            arithEval head_es >>= fun x ->
            mergeHelper tail_es tail_S (acc + headS + (string) x)
        | _, _ -> fail

    mergeHelper es (split s "%") ""

let rec stmntEval (s: stmnt) :  unit stateMonad =
    match s with
    | Skip -> 
        eval {
            return ()    
        }

    | Declare v -> 
        eval {
            //let! a = declare v

            return! declare v
        }

    | Assign(v, a) ->
        eval {
            let! b = arithEval a
            //do! setVar v b
            return! setVar v b
        }

    | Seq(s1, s2) ->
        eval {
            do! stmntEval s1
            return! stmntEval s2
        }


    | If(gaurd, s1, s2) ->
        eval {
            let! b = boolEval gaurd
            if b then 
                return! stmntEval s1 
    
            else 
                return! stmntEval s2 
        }

    | While(guard, s) ->
        eval {
            let! b = boolEval guard

            if b = true then 
                do! stmntEval s   
                return! stmntEval (While(guard, s))
            else  
                return ()
        }

    | Alloc(x, e) -> 
        eval {
            let! size = arithEval e 
            //let! b = getVar x 
            let! _ = getVar x 

            return! alloc x size
        }

    | MemWrite(e1, e2) ->
        eval {
            let! ptr = arithEval e1

            let! v = arithEval e2 

            return! setMem ptr v
        }
        

    | Free(e1, e2) -> 
        eval {
            let! ptr = arithEval e1

            let! size = arithEval e2 

            return! free ptr size
        }

        

    | Print(es, s) ->
        eval {
            let! n = mergeStrings es s 

            return printfn "%A" n

        }
        
;;


let rec arithEval2 (a: aexpr) : int stateMonad =
    match a with
    | Num v -> ret v

    | Var v ->  
        eval{
            let! a = getVar v
            return a
        }


    | Add(ex1, ex2) ->
        eval{
            let! a = arithEval2 ex1 
            let! b = arithEval2 ex2
            return (+) a b
        }

    | Mul(ex1, ex2) ->
        eval{
            let! a = arithEval2 ex1 
            let! b = arithEval2 ex2
            return (a * b)
        }

    | Div(ex1, ex2) ->
        eval{
            let! a = arithEval2 ex1 
            let! b = arithEval2 ex2

            if b <> 0  then return ((/) a b) else return! fail 
        }

    | Mod(ex1, ex2) ->
        eval{
            let! a = arithEval2 ex1 
            let! b = arithEval2 ex2

            if b <> 0  then return ((%) a b) else return! fail 
        }

    | MemRead ex1 -> 
        eval{
            let! a = arithEval2 ex1
            let! b = getMem a

            return b
        }

    | Random -> 
        eval{
            let! a = random
            return a
        }

    | Read ->
        eval{
            let a = readInt ()
            //let! b = a
            return a
        }

    | Cond(b, a1, a2) -> 
        eval{
            let! c = boolEval2 b

            if c then
                let! a = arithEval2 a1
                return a 
            else 
                let! b = arithEval2 a2
                return b 
        }

and boolEval2 (b ) : bool stateMonad =
    match b with
    | TT -> ret true
    | Eq(ex1, ex2) ->
        eval{
            let! a = arithEval2 ex1
            let! b = arithEval2 ex2
            
            return ((=) a b) 
        }

    | Lt(ex1, ex2) ->
        eval{
                let! a = arithEval2 ex1
                let! b = arithEval2 ex2
                
                return ((<) a b) 
        }
    

    | Conj(ex1, ex2) ->
        eval{
                let! a = boolEval2 ex1
                let! b = boolEval2 ex2
                
                return ((&&) a b) 
        }

    | Not ex1 -> 
        eval{
                let! a = boolEval2 ex1
                return (not a) 
        }
    
