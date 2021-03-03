(*
Honor code comes here:

First Name: Jiasheng 
Last Name: Qiu
BU ID: U 80030504

I pledge that this program represents my own
program code and that I have coded on my own. I received
help from no one in designing and debugging my program.
I have read the course syllabus of CS 320 and have read the sections on Collaboration
and Academic Misconduct. I also understand that I may be asked to meet the instructor
or the TF for a follow up interview on Zoom. I may be asked to explain my solution in person and
may also ask you to solve a related problem.
*)

(*
Write a zip function that takes two lists of integers and combines them into a list of pairs of ints
If the two input list are of unequal lengths, return None
your method should be tail recursive.

For example,
safe_zip_int [1;2;3;5] [6;7;8;9] = Some [(1,6);(2,7);(3,8);(5,9)]
safe_zip_int [1] [2;4;6;8] = None
safe_zip_int (between 0 1000000) (between 0 1000000) does not stack overflow
*)


let rec safe_zip_int (ls1: int list) (ls2: int list) : ((int * int) list) option = 
  let rec aux f1 f2 accum=
    match f1,f2 with
    |([],[])->Some []
    |(_::_,[])->None
    |([],_::_)->None

    |(h1::[],h2::[])->  Some(List.rev((h1,h2)::accum))
    |(h1::t1),(h2::t2)-> aux t1 t2 ((h1, h2) :: accum )
  in aux ls1 ls2 []



    (*
    Write a zip function that produces the ith Pell number:
    https://en.wikipedia.org/wiki/Pell_number
    https://oeis.org/A000129
    your function should be tail recursive, but only needs to have the correct output up to integer overflow

    pell 0 = 0
    pell 1 = 1
    pell 7 = 169
    pell 1000000  does not stack overflow
   *)


let rec pell (i: int) : int = 
  if i<=1 then i
  else
    let rec aux num m n =
      if num<=0 then m
      else 
        aux (num-1) n (m+ (2 *n))
    in aux i 0 1

(* The nth Tetranacci number T(n) is mathematically defined as follows.
 *
 *      T(0) = 0
 *      T(1) = 1
 *      T(2) = 1
 *      T(3) = 2
 *      T(n) = T(n-1) + T(n-2) + T(n-3) + T(n-4)
 *
 * For more information, you may consult online sources.
 *
 *    https://en.wikipedia.org/wiki/Generalizations_of_Fibonacci_numbers
 *    https://mathworld.wolfram.com/TetranacciNumber.html
 *
 * Write a tail recursive function tetra that computes the nth Tetranacci
 * number efficiently.In particular, large inputs such as (tetra 1000000)
 * should neither cause stackoverflow nor timeout.
*)

let tetra (n : int) : int = 
  if n=0 then 0 
  else if n=1 then 1
  else if n=2 then 1 
  else if n=3 then 2
  else 
    let rec aux i m n o r=
      if i<=0 then m 
      else aux (i-1) n o r (n+m+o+r) 
    in aux n 0 1 1 2 


    (*
    infinite precision natural numbers can be represented as lists of ints between 0 and 9

    Write a function that takes an integer and represents it with a list of integers between 0 and 9 where the head ias the least signifigant digit.
    If the input is negative return None

    toDec 1234 = Some [4; 3; 2; 1]
    toDec 0 = Some []
    toDec -1234 = None
   *)

(* Hint use 
   mod 10
   / 10
*)

let rec toDec (i : int) : int list option = 
  if i=0 then Some []
  else if i<0 then None
  else 
    let rec aux num accum=
      if num=0 then Some accum
      else aux (num/10) (accum @ [(num mod 10)] ) 
    in aux i []

let lengtht l=
  let rec aux accum l=
    match l with
      [] -> accum
    |
      head::tail -> aux (accum+1) tail
  in 
  aux 0 l

(* same as the previous function toDec but without the option keyword or restrictions
    instead it returns a int list;
    in case of 0 it returns a [];
    there would not be any negative case*)
let rec toDecIntN (i : int) (x:int list) (y:int list) : int list  = 
  if i=0 then 
    if (lengtht x)<(lengtht y) then y
    else x
  else 
    let rec aux num accum=
      if num=0 then  accum
      else aux (num/10) ((num mod 10):: accum) 
    in aux i []

(*Similar to toDec but it recombines the int lists into a int value;
  where the int lists are reversed first,
  then recombined;
  then return the sum of the int values associated with the int lists*)
let comb (a:int list) (b:int list): int=
  let rec aux ls1 ls2 n1 n2=
    match ls1,ls2 with 
    |
      ([],[])-> (n1+n2)
    |
      ([],h2::t2)->aux [] t2 n1 ((n2*10)+h2)
    |
      (h1::t1,[])-> aux t1 [] ((n1*10)+h1) n2
    |
      (h1::t1,h2::t2)-> aux t1 t2 ((n1*10)+h1) ((n2*10)+h2)
    (*using List.rev to reverse the int lists because they are in the reverse order of 
      corresponding int values*)
  in aux (List.rev(a)) (List.rev(b)) 0 0

    (*
    Write a function that sums 2 natrual numbers as represented by a list of integers between 0 and 9 where the head ias the least signifigant digit.
    Your function should be tail recursive

    sum [4; 3; 2; 1] [1;0;1] = [5; 3; 3; 1]
    sum [1] [9;9;9] = [0; 0; 0; 1]
    sum [] [] = []
    sum (nines 1000000) [1] does not stack overflow, when (nines 1000000) provides a list of 1000000 9s
   *)

let rec sum (a : int list) (b : int list) : int list =
  match a,b with
  | [],[]-> [] (* since it only happens when both lists are empty*)
  | [],(h::t)-> b
  | (h::t),[]->a
  | _,_->List.rev(toDecIntN (comb a b) a b)
(* calling comb to recombine and add the int values, then using 
   toDecInt to separate the values*)


    (*
    Write an infinite precision version of the pel function from before

    pell2 0 = []
    pell2 1 = [1]
    pell2 7 = [9; 6; 1]
    pell2 50 = [2; 2; 5; 3; 5; 1; 4; 2; 9; 2; 4; 6; 2; 5; 7; 6; 6; 8; 4]

   *)



let negPos (ls:int list): int list=
  match ls with
  | []->[]
  | h::t -> (h*(-1))::t


let rec toDecInt (i : int): int list  = 
  if i=0 then [0]
  else if i<0 then 
    let rec ba num accum=
      if num=0 then List.rev(negPos(accum))
      else ba (num/10) ((num mod 10):: accum)
    in ba (i*(-1)) []
  else 
    let rec aux num accum=
      if num=0 then  List.rev(accum)
      else aux (num/10) ((num mod 10):: accum) 
    in aux i []


let rec pell2 (i: int) : int list = 
  let aux num accum=
    if num=0 then accum
    else
      toDecInt(num)
  in aux (pell i) []



let _=
  print_int(1)