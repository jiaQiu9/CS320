(*inductive types*)
let rec fold_right f ls b=
  match ls with
    []->b
  | 
    h::t-> f h (fold_right f t b)


let rec fold_left f a l=
  match l with 
    []->a
  |
    h::t->fold_left f (f a h) t


let getH l =List.map (fun subList-> List.nth subList 0) l

let flip f y x=f x y

let getH2 l =List.map (flip List.nth  0) l

type nat = Zero | Succ of nat 
(* let us create our first natural number 0*)
(*let z =Zero*)

let rec convert_nat_to_int n=
  match n with 
    Zero->0
  |
    Succ x->1+convert_nat_to_int x

type 'a binaryTree=Empty | Node of ('a binaryTree * 'a  * 'a binaryTree )

type binaryTree=Empty | Node of (binaryTree * int * binaryTree )
