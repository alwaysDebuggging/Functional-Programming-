module Interpreter.Parser

    open Interpreter.Language

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open FParsecLight.TextParser            // Example parser combinator library. Use for CodeJudge.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.                              x                       [1]                                   x       [1]            


    let pif       : Parser<string> = pstring "if"
    let pelse     : Parser<string> = pstring "else"
    let palloc    : Parser<string> = pstring "alloc"
    let pfree     : Parser<string> = pstring "free"
    let pwhile    : Parser<string> = pstring "while"
    let pdo       : Parser<string> = pstring "do"
    let pdeclare  : Parser<string> = pstring "declare"
    let ptrue     : Parser<string> = pstring "true"
    let pfalse    : Parser<string> = pstring "false"
    let pprint    : Parser<string> = pstring "print"
    let prandom   : Parser<string> = pstring "random"
    let pread     : Parser<string> = pstring "read"
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
    let squareBrackets p = pchar '[' >*>. p .>*> pchar ']'

    let toString (lst: char list) = new string [|for s in lst -> s|]
    
    let parseString = between (pchar '"') (pchar '"') (many(satisfy(fun chr -> chr <> '"'))) |>> toString 
                        |>> fun (str: string) ->  str.Replace("\\n", "\n").Replace("\\t", "\t")
        

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

    // a1 ternaty a2 term a3 prod a5 atom 

    let TernaryParse, terPtref   = createParserForwardedToRef<aexpr>()
    let TermParse, tref = createParserForwardedToRef<aexpr>()
    let ProdParse, pref = createParserForwardedToRef<aexpr>()
    let AtomParse, aref = createParserForwardedToRef<aexpr>()
    

    // 8.10 bool
    let BTermParse, Btref = createParserForwardedToRef<bexpr>()
    let BAtomParse, Baref = createParserForwardedToRef<bexpr>()

    
    let paexpr = TernaryParse
    let pbexpr = BTermParse


    //Level: 1
    let condExpressionParse = pbexpr .>*> pchar '?' .>*>. paexpr .>*> pchar ':' .>*>. paexpr 
                            |>> fun ((a, b), c) -> Cond (a, b, c )
    do terPtref := choice[condExpressionParse; TermParse]


    //Level: 2
    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"

    let SubParse = binop (pchar '-') ProdParse TermParse |>> (fun (a,  b) ->  (.-.) a b ) <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]


    //Level: 3
    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"

    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"

    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]


    //Level: 4
    let NegetiveNumber = unop (pchar '-') AtomParse |>> (fun n -> Mul(Num -1, n)) <?> "NegationNumber"

    let NParse   = pint32 |>> Num <?> "Int"

    let ParParse = parenthesise paexpr <?> "ParenthesisParse"

    let VariableParse = pid |>> Var <?> "Variable"
    
    let ReadParse = pread |>> (fun _ -> Read) <?> "Read"

    let MemRead = squareBrackets paexpr |>> MemRead <?> "MemRead"

    let RandomNumbers = prandom |>> (fun _ -> Random) <?> "RandomNumber"

    do aref := choice [NegetiveNumber; NParse; ReadParse; RandomNumbers; MemRead; ParParse; VariableParse;]



    //Level-1

    let ConjParse = binop (pstring "/\\") BAtomParse BTermParse |>>(fun (b1, b2 )-> b1 .&&. b2 )<?> "Conj"

    let OrParse = binop (pstring "\\/") BAtomParse BTermParse |>> (fun (b1, b2) -> b1 .||. b2 ) <?> "or"

    let EqualParse = binop (pchar '=') TermParse paexpr |>> Eq <?> "Equal"

    let NotEqualToParse = binop (pstring "<>") TermParse paexpr |>> (fun (a, b) -> a .<>. b) <?> "NotEqualTo"

    let LessThanParse = binop (pchar '<') TermParse paexpr |>> (fun (a, b ) -> a .<. b) <?> "LessThan"

    let GreaterThanParse = binop (pchar '>') TermParse paexpr |>> (fun (a, b ) -> a .>. b) <?> "GreaterThan"

    let LessOrEqualToParse = binop (pstring "<=") TermParse paexpr |>> (fun (a, b ) -> a .<=. b) <?> "LessOrEqualToParse"

    let GreaterOrEqualToParse = binop (pstring ">=") TermParse paexpr |>> (fun (a, b ) -> a .>=. b) <?> "GreaterOrEqualToParse"

    let NottParse = unop (pchar '~') BAtomParse |>> (fun (b ) -> ~~ b) <?> "Nott"


    let TrueParse = ptrue |>> (fun _ -> TT) <?> "True"

    let FalseParse = pfalse |>> (fun _ -> Not TT) <?> "NotTrue"

    let BParParse = parenthesise pbexpr <?> "BoolParenthesisParse"

    do Btref := choice [OrParse; ConjParse; BAtomParse;]
    do Baref := choice [TrueParse; FalseParse; NottParse; EqualParse; NotEqualToParse; LessThanParse; GreaterThanParse; LessOrEqualToParse; GreaterOrEqualToParse; BParParse]

    //Stmnt

    let pstmnt= pstring "not implemented" |>> (fun _ -> Skip)
    let pprogram = pstmnt |>> (fun s -> (Map.empty : program), s)
    
    let run = run
       
    let runProgramParser = run (pprogram .>> eof)  

