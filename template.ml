
(* This file: https://piazza.com/bu/spring2021/cs320/resources *)
(* Zoom: https://bostonu.zoom.us/j/98373892388?pwd=aHR5RzNhMmVvaFNaOW1UckxlY3N6QT09 *)
(* Videos will be available in: https://learn.bu.edu/webapps/blackboard/content/listContentEditable.jsp?content_id=_8868652_1&course_id=_74834_1 *)

(* 
A2 CAS B25B Wed  8:00am  8:50am  Qiancheng "Robin" Fu 
A3 CAS B25B Wed  9:05am  9:55am  Qiancheng "Robin" Fu 
A4 CAS 201  Wed 10:10am 11:00am  Qiancheng "Robin" Fu 
A5 CAS 201  Wed 11:15am 12:05pm  Qiancheng "Robin" Fu 
B2 CAS B06A Wed 12:20pm  1:10pm  Daniel Bastidas 
B3 CAS B06A Wed  1:25pm  2:15pm  Daniel Bastidas 
B4 CAS B06A Wed  2:30pm  3:20pm  Daniel Bastidas 
B5 CAS B06A Wed  3:35pm  4:25pm  Daniel Bastidas 
*)

(* record labs *)

(* simple data structures
 *
 * stack
 * queue
 * trees
 *
*)











(* stack *)

type 'a stack = 'a list

(* stack functions *)

let empty_stack () : 'a stack= []

let push (x : 'a) (st : 'a stack) : 'a stack = x::st

let pop (st : 'a stack) : 'a stack = 
  match st with
    []->[]
  |
    _::st->st

let peek (st : 'a stack) : 'a option = 
  match st with
    []->None
  |
    x::_ -> Some x

(* creating a new stack *)
let x= empty_stack()

(* pushing elements into stack *)
let x = push 1 x
let x = push 2 x
let x = push 3 x
let x = push 4 x
let x = push 5 x
let x = push 6 x
(* removing elements from stack *)

let x =pop x
let x =pop x
(* look at value from stack *)
let x =peek x











(* naive queue *)

type 'a queue = 'a list

(* queue functions *)

let empty_queue () : 'a queue = []

let enqueue (x : 'a) (q : 'a queue) : 'a queue = q @ [x]

let dequeue (q : 'a queue) : 'a queue = 
  match q with
  | []->[]
  | _::q->q

let last (q : 'a queue) : 'a option = 
  match q with
    []->None
  |
    q::_->Some q

(* creating a new queue *)
let x =empty_queue()


(* pushing elements into stack *)
let x= enqueue 1 x
let x= enqueue 2 x
let x= enqueue 3 x
let x= enqueue 4 x
let x= enqueue 5 x
let x= enqueue 6 x
(* removing elements from stack *)
let x=dequeue x
let x=dequeue x
let x=dequeue x
(* look at value from stack *)

let x= peek x









(* Dequeue for naive queues require us to reverse the
 * underlying list twice. This is inefficient, can we
 * do better? *)

(* better queue *)

type 'a queue = ('a list * 'a list)

(* queue functions *)

let empty_queue () : 'a queue = ([], [])

let enqueue (x : 'a) (q : 'a queue) = 
  let (l,r)= q in 
  (x::l,r)

let dequeue (q : 'a queue) : 'a queue = 
  let (l,r) = q in 
  match r with 
    _::r->(l,r)
  |
    []->
    match List.rev l with 
    | _ :: r ->([],r)
    | []->([],[])

let last (q : 'a queue) : 'a option = 
  let (l,r)=q in 
  match r with
  | x::_ -> Some x
  | []->
    match List.rev l with
    |x ::_->Some x
    | []->None
(* creating a new queue *)


(* pushing elements into stack *)


(* removing elements from stack *)

(* look at value from stack *)













(* binary tree *)

type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

(* tree functions *)

let empty_tree () = Leaf

let rec insert_tree (compare : 'a -> 'a -> bool) (x : 'a) (t : 'a tree) = 
  match t with 
  | Leaf -> Node (x, Leaf,Leaf)
  | Node (y, t1,t2)->
    if compare x y then
      let t1= insert_tree compare x t1 in 
      Node (y,t1,t2)
    else
      let t2=insert_tree compare x t2 in 
      Node (y,t1,t2)


let rec flatten (t : 'a tree) : 'a list = 
  match t with 
  |Leaf -> []
  | Node (x,t1,t2)->
    x::(flatten t1) @(flatten t2)

(* "invert binary tree" *)

let rec invert (t : 'a tree) : 'a tree = 
  match t with 
  |Leaf ->Leaf
  | Node (x,l,r) -> 
    let l = invert l in 
    let r = invert r in 
    Node (x,r,l)

(* random list of integers *)

let l = [5; 1; 6; 2; 9; 0; 3; 4; 8; 7]

(* insertion into tree *)

let t =
  List.fold_left
    (fun acc x -> insert_tree (<) x acc)
    (empty_tree ()) l

let invert_t = invert t

(* fold tree *)

let rec fold (f : 'a -> 'b -> 'b -> 'b) (b : 'b) (t : 'a tree) : 'b =
  match t with
  | Leaf -> b
  | Node(x,l,r)-> 
    let v1 = fold f b l in 
    let v2 = fold f b r in 
    f x v1 v2
(* sum integer tree *)

let sum_tree (t : int tree) : int = 
  fold (fun x l r-> x+l+r) 0 t

let n = sum_tree t

let flatten_tree' (t : 'a tree) : 'a list = 
  fold (fun x l r -> x::l @ r) [] t

let l = flatten_tree' t

let invert_tree' (t : 'a tree) : 'a tree = 
  fold (fun x l r -> Node(x,r,l)) Leaf t

let invert_t' = invert_tree' t
