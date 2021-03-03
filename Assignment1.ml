(*
Honor code comes here:

First Name: Jiasheng
Last Name: Qiu
BU ID: U80030504

I pledge that this program represents my own
program code and that I have coded on my own. I received
help from no one in designing and debugging my program.
I have read the course syllabus of CS 320 and have read the sections on Collaboration
and Academic Misconduct. I also understand that I may be asked to meet the instructor
or the TF for a follow up interview on Zoom. I may be asked to explain my solution in person and
may also ask you to solve a related problem.
*)
(** reversing the order of the string*)
let rev l=
  let rec aux accum list=
    match list with
      []->accum
    |
      head::tail->aux (head::accum) tail
  in aux [] l 

(** return the length of a list*)
let lengtht l=
  let rec aux accum l=
    match l with
      [] -> accum
    |
      head::tail -> aux (accum+1) tail
  in 
  aux 0 l


(* 
a print_list function useful for debugging.
*)

let rec print_list (ls: int list): unit =
  let rec aux ls = match ls with
    | [] -> print_string ""
    | e::[] -> print_int e
    | e::l -> 
      let _ = print_int e 
      in let _ = print_string "; " 
      in aux l

  in let _ = print_string "[" 
  in let _ = aux ls
  in         print_string "]" 





(* Problems *)

(*
TODO: Write a function called between that lists the integers between two integers (inclusive)
If the first number is greater then the second return the empty list
the solution should be tail recursive

For example,
between 4 7 = [4; 5; 6; 7]
between 3 3 = [3]
between 10 2 = []
between 4 1000000 does not stack overflow
*)


let between (n:int) (e:int): int list = 
  let rec aux a b ls=
    if a>b then rev ls
    else if a==b then rev(a::ls)
    else 
      aux (a+1) b (a::ls)
  in aux n e []


(*
TODO: Write a zip function that takes two lists of integers and combines them into a list of pairs of ints
If the two input list are of unequal lengths, combine as long as possible
your method should be tail recursive.

For example,
zip_int [1;2;3;5] [6;7;8;9] = [(1,6);(2,7);(3,8);(5,9)]
zip_int [1] [2;4;6;8] = [(1,2)]
zip_int (between 0 1000000) (between 0 1000000) does not stack overflow
*)



let zip_int (a: int list) (b: int list): (int * int) list = 
  let rec f  ls1 ls2 accum=
    match ls1,ls2 with
      [],[]->rev accum
    |
      [],_-> rev accum
    |
      _,[]-> rev accum
    |
      (ls1::tail), (ls2::tails) ->f  (tail) (tails) ( (ls1,ls2)::accum)
  in f a b []

(*
TODO: Write a dotProduct function for lists of integers,
If the two list are of unequal lengths then return 0

For example,
dotProduct [1;2;3;4] [6;7;8;9] = 80            (since 1*6+2*7+3*8+4*9 = 80)
dotProduct [1;2;3;4] [6] = 0
*)

let  dotProduct (x: int list) (y: int list): int =
  let rec aux ls1 ls2 accum= if lengtht(ls1)==lengtht(ls2)then 
      match ls1,ls2 with
        [],_->0
      |
        _,[]->0
      | 
        (ls1::tail), (ls2::tails) -> aux  (tail) (tails) accum+(ls1*ls2)
    else 0
  in aux x y 0




(* 
TODO:
Write a function that takes a list of tuples and returns a string representation of that list

your representation should be valid as OCaml source:
* every element of a list must be separated by ";"
* the list must be wrapped in "[" and "]"
* tuples should (1,2)
* You may use whitespace however you like

For example,
list_of_tuple_as_string [(1,2);(3,4);(5,6)] = "[ (1,2); (3,4); (5,6) ]"
*)


let rec list_of_tuple_as_string (lst: (int*int) list): string =
  let rec aux ls accum=
    match ls with
    | []->(accum^"]")
    |(e1,e2)::l->aux l (if l==[] then accum^" ("^string_of_int(e1)^","^string_of_int(e2)^")"^" " 
                        else accum^" ("^string_of_int(e1)^","^string_of_int(e2)^")"^"; ")

  in aux lst "["


(* 
TODO:
Write an insertion sort function for lists of integers

for example,
sort [6;7;1] = [1;6;7]
*)

(* 
Hint: We encourage you to write the following helper function 

let rec insert (i: int) (list: int list): int list = failwith "unimplemented"

that takes a a number, an already sorted ls and returns a new sorted list with that number inserted
for example,
insert 5 [1;3;5;7] = [1;3;5;5;7]

You can  then call this helper function inside sort. 
*)

let rec insert (i: int) (lst: int list): int list = 
  let rec aux lso lsn inum=
    match lso with
    |[]->(rev (inum::lsn)) 
    |e::l -> (if inum<=e then ( List.append (rev lsn) (inum::lso))  
              else (aux l (e::lsn) inum ))

  in aux lst [] i



let rec sort (ls: int list): int list = 
  let rec aux lst accum=
    match lst with 
    |[]->accum
    |e::l-> aux l ( (insert e accum))
  in aux ls []


