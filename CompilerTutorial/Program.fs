// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open lexer
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Text.Lexing

type id = string
type binop = Plus | Minus | Times | Div
type stm = 
    | CompoundStm of stm * stm
    | AssignStm of id * exp
    | PrintStm of exp list 
and exp = 
    | IdExp of id
    | NumExp of int
    | OpExp of exp * binop * exp
    | EseqExp of stm * exp



let rec maxargs = function
    | CompoundStm (fst, snd) -> max (maxargs fst)  (maxargs snd)
    | AssignStm (id, exp) -> 0
    | PrintStm xs -> List.length xs


let apply (a : int) binop (b : int) = 
    match binop with
    | Plus -> a + b
    | Minus -> a - b
    | Times -> a * b
    | Div -> a /b

let rec interpStm (tbl : Map<id, int>) stm =
    match stm with
    | CompoundStm (fst, snd) -> 
        let tbl' = interpStm tbl fst
        interpStm tbl' snd
    | AssignStm (id, exp) -> 
        let tbl', newval = (interpExp tbl exp)
        Map.add id newval tbl'
    | PrintStm (x::xs) -> 
        let tbl', i = interpExp tbl x
        printfn "%d" i
        let p = PrintStm (xs)
        interpStm tbl' p
    |PrintStm ([]) -> 
        tbl
and interpExp tbl exp = 
    match exp with 
    | IdExp id -> (tbl, Map.find id tbl)
    | NumExp i -> (tbl, i)
    | OpExp (a, op, b) -> 
        let tbla, ai = interpExp tbl a
        let tblb, bi = interpExp tbl b
        (tbl, (apply ai op bi))
    | EseqExp (stm, exp) -> 
        let tbl' = interpStm tbl stm
        let tbl'', i = interpExp tbl' exp 
        (tbl'', i)

let interp stm = 
    let tbl = Map.empty
    interpStm tbl stm


let testStm1 = PrintStm [ NumExp(4);NumExp(5) ]
let testStm2 = CompoundStm(AssignStm("a", NumExp(11)), CompoundStm(AssignStm("b", NumExp(12)), PrintStm [ OpExp(IdExp("a"), Plus, IdExp("b")) ]))

let test1 = interp testStm1 
let test2 = interp testStm2



// Exercise 1.1
//
type key = Str

printfn "A Test"

let rec toTokenList tokenList tokenbuf = 
    let next = lexer.tokenize tokenbuf
    match next with
    | Eof -> tokenList
    | _ as t -> 
        let newlist = t :: tokenList
        toTokenList newlist tokenbuf

let toTokens = 
    let empty = []
    toTokenList empty


[<EntryPoint>]
let main argv = 
    let lb = LexBuffer<char>.FromString "( 1 + 2 )"
    let tokens = toTokens lb

    printfn "Press any key to continue..."
    System.Console.ReadLine() |> ignore
    0 // return an integer exit code
