module Exercises_Chapter_1_1

type key = string
type tree =
    | Leaf 
    | Tree of tree * key * tree

let empty = Leaf

let rec insert key = function
    | Leaf -> Tree (Leaf, key, Leaf)
    | Tree (l, k, r)  ->
        if key < k then Tree((insert key l), k, r)
        else if key > k then Tree(l, k, (insert key r))
        else Tree(l, k, r)


//
// 1.1 a
// Implement contains which determines if
// the tbl contains the key
//
let rec contains key = function
    | Leaf -> false
    | Tree (l, k, r) ->
        if key < k then contains key l
        else if key > k then contains key r 
        else true



