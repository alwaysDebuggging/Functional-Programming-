module Interpreter.Parser

    open Interpreter.Language

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open JParsec.TextParser             // Example parser combinator library. Use for CodeJudge.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.                              x                       [1]                                   x       [1]            


    let pif       : Parser<string> = pstring "if"
    let pelse     : Parser<string> = pstring "else"
    let palloc    : Parser<string> = pstring "alloc"
    let pfree     : Parser<string> = pstring "free"
    let pwhile    : Parser<string> = pstring "while"
    let pdo       : Parser<string> = pstring "do"
    let pdeclare  : Parser<string> = pstring "declare"
    let ptrue     : Parser<string> = pstring "true "
    let pfalse    : Parser<string> = pstring "false "
    let pprint    : Parser<string> = pstring "print"
    let prandom   : Parser<string> = pstring "random "
    let pread     : Parser<string> = pstring "read "
    let pfunction : Parser<string> = pstring "function"
    let pret      : Parser<string> = pstring "ret"
    
    let pwhitespaceChar = satisfy System.Char.IsWhiteSpace
    let pletter = satisfy System.Char.IsLetter
    let palphanumeric = satisfy System.Char.IsLetterOrDigit

    let spaces = many pwhitespaceChar //pwhitespaceChar  |>> fun x -> [x]
    let spaces1 = many1 pwhitespaceChar //pwhitespaceChar |>> fun x -> [x] // or this many1 pwhitespaceChar or this many pwhitespaceChar

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2 = p1 .>> spaces .>> p2
    let (>*>.) p1 p2 = p1 .>> spaces >>. p2

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'
    let curlybrackets p = pchar '{' >*>. p .>*> pchar '}'

    let toString (lst: char list) = new string [|for s in lst -> s|]
    
    // let parseString : Result<string, string> = 
    //     pletter 

    //     str.Replace(, <with this substring>)
    //     new string [|for c in chars -> c|]

    let pid  = 
        pletter <|> pchar '_' .>>.  many(palphanumeric <|> pchar '_') |>> fun (ch, chlst) -> toString (ch :: chlst)

        // pletter <|> pchar '_' .>>.  many(palphanumeric <|> pchar '_') |>> fun (ch, chlst: char list) -> new string [|for s in ch::chlst -> s|]

        
        // let check = pletter <|> pchar '_' .>>.  many(palphanumeric <|> pchar '_')

        // let tost = check |>> fun (c, cs: char list) -> c::cs

        // let mapCC   =  tost |>> fun ls -> 
        // mapCC

    // run pid "x1"                                x                       [1]                                   x       [1]            

    
    let unop op a  = op >*>. a
    let binop op a b = a .>*> op .>*>. b 


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

    let paexpr = pstring "not implemented" |>> (fun _ -> Num 42)

    let pbexpr = pstring "not implemented" |>> (fun _ -> TT)

    let pstmnt = pstring "not implemented" |>> (fun _ -> Skip)
    
    let pprogram = pstmnt |>> (fun s -> (Map.empty : program), s)
    
    let run = run
       
    let runProgramParser = run (pprogram .>> eof)  

