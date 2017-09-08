module Exercises_Chapter_1_2


type key = string
type value = string
type binding = key * value
type tree =
    | Leaf 
    | Tree of tree * binding * tree

let empty = Leaf

//
// Insert key 
//
let rec insert key value = function
    | Leaf -> Tree (Leaf, (key, value), Leaf)
    | Tree (l, b, r)  ->
        let k, v = b
        let newb = (key, value)
        if key < k then Tree((insert key value l), b, r)
        else if key > k then Tree(l, b, (insert  key value r))
        else Tree(l, newb, r)


//
// Contains
// Does the table contain a binding with the key
//
let rec contains key = function
    | Leaf -> false
    | Tree (l, (k,v), r) ->
        if key < k then contains key l
        else if key > k then contains key r 
        else true



let rec lookup key = function
    | Leaf -> None
    | Tree (l, (k, v), r) ->
        if key < k then lookup key l
        else if key > k then lookup key r
        else Some v



