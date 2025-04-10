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

    let A1, A1ref = createParserForwardedToRef<aexpr>()
    let A2, A2ref = createParserForwardedToRef<aexpr>()
    let A3, A3ref = createParserForwardedToRef<aexpr>()
    let A4, A4ref = createParserForwardedToRef<aexpr>()
    

    // 8.10 bool
    let B1, B1ref = createParserForwardedToRef<bexpr>()
    let B2, B2ref = createParserForwardedToRef<bexpr>()

    
    let paexpr = A1
    let pbexpr = B1


    //Level: 1
    let condExpressionParse = pbexpr .>*> pchar '?' .>*>. paexpr .>*> pchar ':' .>*>. paexpr 
                            |>> fun ((a, b), c) -> Cond (a, b, c )
    do A1ref := choice[condExpressionParse; A2]


    //Level: 2
    let AddParse = binop (pchar '+') A3 A2 |>> Add <?> "Add"

    let SubParse = binop (pchar '-') A3 A2 |>> (fun (a,  b) ->  (.-.) a b ) <?> "Sub"
    do A2ref := choice [AddParse; SubParse; A3]


    //Level: 3
    let MulParse = binop (pchar '*') A4 A3 |>> Mul <?> "Mul"

    let DivParse = binop (pchar '/') A4 A3 |>> Div <?> "Div"

    let ModParse = binop (pchar '%') A4 A3 |>> Mod <?> "Mod"
    do A3ref := choice [MulParse; DivParse; ModParse; A4]


    //Level: 4
    let NegetiveNumber = unop (pchar '-') A4 |>> (fun n -> Mul(Num -1, n)) <?> "NegationNumber"

    let NParse   = pint32 |>> Num <?> "Int"

    let ParParse = parenthesise paexpr <?> "ParenthesisParse"

    let VariableParse = pid |>> Var <?> "Variable"
    
    let ReadParse = pread |>> (fun _ -> Read) <?> "Read"

    let MemReadParse = squareBrackets paexpr 
                    |>> MemRead <?> "MemRead"

    let RandomNumbers = prandom |>> (fun _ -> Random) <?> "RandomNumber"

    do A4ref := choice [NegetiveNumber; NParse; ReadParse; RandomNumbers; MemReadParse; ParParse; VariableParse;]



    //Level-1

    let ConjParse = binop (pstring "/\\") B2 B1 |>>(fun (b1, b2 )-> b1 .&&. b2 )<?> "Conj"

    let OrParse = binop (pstring "\\/") B2 B1 |>> (fun (b1, b2) -> b1 .||. b2 ) <?> "or"

    let EqualParse = binop (pchar '=') A2 paexpr |>> Eq <?> "Equal"

    let NotEqualToParse = binop (pstring "<>") A2 paexpr |>> (fun (a, b) -> a .<>. b) <?> "NotEqualTo"

    let LessThanParse = binop (pchar '<') A2 paexpr |>> (fun (a, b ) -> a .<. b) <?> "LessThan"

    let GreaterThanParse = binop (pchar '>') A2 paexpr |>> (fun (a, b ) -> a .>. b) <?> "GreaterThan"

    let LessOrEqualToParse = binop (pstring "<=") A2 paexpr |>> (fun (a, b ) -> a .<=. b) <?> "LessOrEqualToParse"

    let GreaterOrEqualToParse = binop (pstring ">=") A2 paexpr |>> (fun (a, b ) -> a .>=. b) <?> "GreaterOrEqualToParse"

    let NottParse = unop (pchar '~') B2 |>> (fun (b ) -> ~~ b) <?> "Nott"


    let TrueParse = ptrue |>> (fun _ -> TT) <?> "True"

    let FalseParse = pfalse |>> (fun _ -> Not TT) <?> "NotTrue"

    let BParParse = parenthesise pbexpr <?> "BoolParenthesisParse"

    do B1ref := choice [OrParse; ConjParse; B2;]
    do B2ref := choice [TrueParse; FalseParse; NottParse; EqualParse; NotEqualToParse; LessThanParse; GreaterThanParse; LessOrEqualToParse; GreaterOrEqualToParse; BParParse]

    //Stmnt

    let S1, s1Ref = createParserForwardedToRef<stmnt>()
    let S2, s2Ref = createParserForwardedToRef<stmnt>()

    let pstmnt = S1

    //level-1
    let SemiParse = binop (pchar ';') S2 S1 |>> (fun (a, b) -> Seq (a, b)) <?> "Sequence"


    //level-2

    let AssignParsePid = binop (pstring ":=") pid A1
                        |>> (fun (a, b)-> Assign(a, b) )<?> "Assign" 

    let DeclareParse = pdeclare >>. spaces1 >>. pid 
                    |>> (fun v -> Declare v ) <?> "declare"

    let IfThenElseBlockParse = pif .>> spaces1 >>. parenthesise(B1) .>> spaces1 .>>. curlybrackets(S1) .>> spaces1 .>> pelse .>> spaces1 .>>. curlybrackets(S1) 
                            |>> (fun ((B1, S1), S2) -> If(B1, S1, S2)) <?> "IfElse"

    let IfParse = pif .>> spaces1 >>. parenthesise(B1) .>> spaces1 .>>. curlybrackets(S1)
                |>> (fun (a, b) -> IT(a, b)) <?> "If"

    let WhileParse = pwhile >>. spaces1 >>. parenthesise(B1) .>> spaces1 .>>. curlybrackets(S1)
                    |>> (fun (B1, S1) -> While(B1,S1)) <?> "While"

    let AllocParse = palloc >*>. parenthesise(binop (pchar ',') pid  A1) 
                    |>> (fun (a, b) -> Alloc(a, b)) <?> "alloc"

    let FreeParse = pfree >*>. parenthesise(binop (pchar ',') A1 A1) 
                |>> (fun (a, b) -> Free(a, b)) <?> "free"
    
    let PrintParse = pprint >*>. parenthesise(parseString .>*>. many1(pchar ',' >*>. A1)) 
                    |>> (fun (a, b) -> Print(b, a)) <?> "Print"

    let AssignParsePMem = binop (pstring ":=") (squareBrackets A1)  A1
                                |>> (fun (a, b) -> MemWrite(a, b)) <?> "MemAssign"


    do s1Ref := choice [SemiParse; S2]
    do s2Ref := choice [IfThenElseBlockParse; IfParse; WhileParse; AssignParsePMem; AssignParsePid; FreeParse; AllocParse; FreeParse; DeclareParse; PrintParse;]


    let pprogram = pstmnt |>> (fun s -> (Map.empty : program), s)
    
    let run = run
       
    let runProgramParser = run (pprogram .>> eof)  

