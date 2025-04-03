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

<<<<<<< Updated upstream
<<<<<<< Updated upstream
    
    let unop _ = failwith "not implemented"
    let binop _ = failwith "not implemented"

    let TermParse, tref = createParserForwardedToRef<aexpr>()
    let ProdParse, pref = createParserForwardedToRef<aexpr>()
    let AtomParse, aref = createParserForwardedToRef<aexpr>()
=======
let parenthesise p = pchar '(' >*>. p .>*> pchar ')'
let curlybrackets p = pchar '{' >*>. p .>*> pchar '}'
=======
let parenthesise p = pchar '(' >*>. p .>*> pchar ')'
let curlybrackets p = pchar '{' >*>. p .>*> pchar '}'

let toString (lst: char list) = new string [|for s in lst -> s|]
let parseString : Result<string, string> = 
    pletter 

    str.Replace(, <with this substring>)
    new string [|for c in chars -> c|]

let pid  = 
    pletter <|> pchar '_' .>>.  many(palphanumeric <|> pchar '_') |>> fun (ch, chlst) -> toString (ch :: chlst)

    // pletter <|> pchar '_' .>>.  many(palphanumeric <|> pchar '_') |>> fun (ch, chlst: char list) -> new string [|for s in ch::chlst -> s|]

    
    // let check = pletter <|> pchar '_' .>>.  many(palphanumeric <|> pchar '_')

    // let tost = check |>> fun (c, cs: char list) -> c::cs

    // let mapCC   =  tost |>> fun ls -> 
    // mapCC

// run pid "x1"                                x                       [1]                                   x       [1]            
>>>>>>> Stashed changes

let toString (lst: char list) = new string [|for s in lst -> s|]
let parseString : Result<string, string> = 
    pletter 

<<<<<<< Updated upstream
    str.Replace(, <with this substring>)
    new string [|for c in chars -> c|]
=======
let unop op a  = op >*>. a
let binop op a b = a .>*> op .>*>. b  
>>>>>>> Stashed changes

let pid  = 
    pletter <|> pchar '_' .>>.  many(palphanumeric <|> pchar '_') |>> fun (ch, chlst) -> toString (ch :: chlst)

    // pletter <|> pchar '_' .>>.  many(palphanumeric <|> pchar '_') |>> fun (ch, chlst: char list) -> new string [|for s in ch::chlst -> s|]

    
    // let check = pletter <|> pchar '_' .>>.  many(palphanumeric <|> pchar '_')

    // let tost = check |>> fun (c, cs: char list) -> c::cs

    // let mapCC   =  tost |>> fun ls -> 
    // mapCC

// run pid "x1"                                x                       [1]                                   x       [1]            
>>>>>>> Stashed changes

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    do tref := choice [AddParse; ProdParse]

<<<<<<< Updated upstream
    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    do pref := choice [MulParse; AtomParse]
=======
let unop op a  = op >*>. a
let binop op a b = a .>*> op .>*>. b  
>>>>>>> Stashed changes

    let NParse   = pint32 |>> Num <?> "Int"
    let ParParse = parenthesise TermParse
    do aref := choice [NParse; ParParse]

    let paexpr = pstring "not implemented" 

    let pbexpr = pstring "not implemented"

    let pstmnt = pstring "not implemented" |>> (fun _ -> Skip)
    
    let pprogram = pstmnt |>> (fun s -> (Map.empty : program), s)
       
    let runProgramParser = run (pprogram .>> eof)  
