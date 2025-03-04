module Interpreter.State
    
    type state;;
    
    val mkState : unit -> state;;
    
    val declare : string -> state -> state option;;
    
    val getVar : string -> state -> int option;;
    
    val setVar : string -> int -> state -> state option;;
