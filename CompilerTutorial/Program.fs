// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Microsoft.FSharp.Collections

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
let testStm2 = CompoundStm(AssignStm("a", NumExp(11)), PrintStm [ IdExp("a") ])

let test1 = interp testStm1 
let test2 = interp testStm2

printfn "A Test"

[<EntryPoint>]
let main argv = 
    testStm1 |> ignore
    0 // return an integer exit code
