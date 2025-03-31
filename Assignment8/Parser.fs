module Interpreter.Parser

    open Interpreter.Language

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open JParsec.TextParser             // Example parser combinator library. Use for CodeJudge.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "read"
    let pPointValue = pstring "random"

    let pCharToInt  = pstring "true"
    let pToUpper    = pstring "not implemented"
    let pToLower    = pstring "not implemented"
    let pCharValue  = pstring "not implemented"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "not implemented"
    let pIsLetter   = pstring "not implemented"
    let pIsVowel   = pstring "not implemented"

    let pif       = pstring "if"
    let pthen     = pstring "not implemented"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "not implemented"
    let pdeclare  = pstring "declare"

    let whitespaceChar = pstring "not implemented"
    let pletter        = pstring "not implemented"
    let palphanumeric  = pstring "not implemented"

    let spaces         = pstring "not implemented"
    let spaces1        = pstring "not implemented"

    let (.>*>.) _ _ = failwith "not implemented"
    let (.>*>) _ _  = failwith "not implemented"
    let (>*>.) _ _  = failwith "not implemented"

    let parenthesise p = p // incorrect (not implemented)

    let pid = pstring "not implemented"

    
    let unop _ = failwith "not implemented"
    let binop _ = failwith "not implemented"

    let TermParse, tref = createParserForwardedToRef<aexpr>()
    let ProdParse, pref = createParserForwardedToRef<aexpr>()
    let AtomParse, aref = createParserForwardedToRef<aexpr>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    do tref := choice [AddParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    do pref := choice [MulParse; AtomParse]

    let NParse   = pint32 |>> Num <?> "Int"
    let ParParse = parenthesise TermParse
    do aref := choice [NParse; ParParse]

    let paexpr = pstring "not implemented" 

    let pbexpr = pstring "not implemented"

    let pstmnt = pstring "not implemented" |>> (fun _ -> Skip)
    
    let pprogram = pstmnt |>> (fun s -> (Map.empty : program), s)
       
    let runProgramParser = run (pprogram .>> eof)  
