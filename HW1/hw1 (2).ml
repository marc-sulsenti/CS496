(* Marc Sulsenti  CS496  Homework One
   May 21st 2024
   "I pledge my honor that I have abided by the Stevens Honor System"*)

(*Starter Varaibles  from HW1.PDF*)

type program = int list

let square : program = [0; 2; 2; 3; 3; 4; 4; 5; 5; 1]
let letter_e : program = [ 0 ; 2 ; 2 ; 3 ; 3 ; 5 ; 5 ; 4 ; 3 ; 5 ; 4 ; 3 ; 3 ; 5 ; 5 ; 1 ]


let letter_e = [ 0 ; 2 ; 2 ; 3 ; 3 ; 5 ; 5 ; 4 ; 3 ; 5 ; 4 ; 3 ; 3 ; 5 ; 5 ; 1 ]
(* Task One Mirror Image*)

(** [mirror_image]  returns a program that draws the mirror image of the input program. 
    Example:
    mirror_image letter_e ;;
   : int list = [0; 4; 4; 5; 5; 3; 3; 2; 5; 3; 2; 5; 5; 3; 3; 1] *)
let mirror_help i = 
  match i with
  | 0 -> 0
  | 1 -> 1
  | 2 -> 4
  | 3 -> 5
  | 4 -> 2
  | 5 -> 3
  | _ -> failwith "not a direction"

    let rec mirror_image l = List.map mirror_help l

(* Task Two  Rotate Letter 90*)
(** [rotate_90_letter]  given a program returns a new one which draws the same pictures except that
they are rotated 90 degrees. 
Example:
rotate_90_letter letter_e ;;
2 - : int list = [0; 3; 3; 4; 4; 2; 2; 5; 4; 2; 5; 4; 4; 2; 2; 1]*)
let rotate90_help  i = 
  match i with
  | 0 -> 0
  | 1 -> 1
  | 2 -> 3
  | 3 -> 4
  | 4 -> 5
  | 5 -> 2
  | _ -> failwith "not a direction"

    let rotate_90_letter l = List.map(rotate90_help) l

(* Task Three Rotate Word 90*)

(** [rotate_90_word] given a list of programs representing a letter, return a new list that rotates the word by 90 degrees
    Example:
    # rotate _90_word [ letter_e ; letter_e ];;
   int list list =  [[0; 3; 3; 4; 4; 2; 2; 5; 4; 2; 5; 4; 4; 2; 2; 1];  [0; 3; 3; 4; 4; 2; 2; 5; 4; 2; 5; 4; 4; 2; 2; 1]]*)
let rotate90_help  i = 
  match i with
  | 0 -> 0
  | 1 -> 1
  | 2 -> 3
  | 3 -> 4
  | 4 -> 5
  | 5 -> 2
  | _ -> failwith "not a direction"

let rotate_90_word  l = List.map(List.map(rotate90_help ) ) l


(* Task Four Repeat List*)
(** [repeat]  takes parameters int i, 'a s , and returns 'a list with 'a repeated i times in that list
    Example:
    # repeat 3 " hello " ;;
  string list = [ " hello " ; " hello " ; " hello " ]*)


let  rec repeat : int -> 'a  -> 'a list =
fun i s ->
match i with
| 0 -> []
| _ -> s :: repeat ( i -  1 )  s



(* Task Five Pantograph*)
(* NOTE - One solution using map, No map, and fold (Three total solutions)  *)

(** [pantograph] takes int n and int list p, and draws the program p, enlarged n fold times
    [pantograph_nm] pantograph, but without utilizing List.map
    [pantograph_fold] pantograph, but utilizing List.fold_left
    # pantograph 2 letter_e ;;
  : int list =
  [0; 2; 2; 2; 2; 3; 3; 3; 3; 5; 5; 5; 5; 4; 4; 3; 3; 5; 5; 4; 4; 3; 3; 3; 3; 5; 5; 5; 5; 1]*) 

(*Helper Function*)
let  rec repeat : int -> 'a  -> 'a list =
fun i s ->
match i with
| 0 -> []
| _ -> s :: repeat ( i -  1 )  s

(* MAIN SOLUTION UTILIZING LIST.MAP*)
let rec pantograph : int -> int list -> int list =
  fun i l ->
    match l with
    | [] -> []
    | h :: t ->
      if h = 0 || h = 1 then
        h :: pantograph i t
      else
        List.concat (List.map (repeat i) [h]) @ pantograph i t

(*No Map only matching*)
let rec pantograph_nm: int -> int list -> int list =
    fun  i l -> 
    match l with
    | [] -> []
    | h :: t -> 
      if h = 0 || h = 1 then 
       h :: pantograph_nm i t
    else
      repeat i h  @ pantograph_nm i t

(* Using fold *)

let rec pantograph_fold : int -> int list -> int list =
  fun i l ->
    let result = List.fold_left (fun acc h ->
      if h = 0 || h = 1 then
        h :: acc
      else
         List.rev_append (repeat i h) acc
    ) [] l in
    List.rev result

(* Task Six Coverage *)

(** [coverage] coverage taking a program, returns the list of coordinates (x,y) that the program visits. Repititons
    are not to be concerned with.
    # coverage (0 ,0) letter_e ;;
2 - : ( int * int ) list =
 [(0 , 0) ; (0 , 0) ; (0 , 1) ; (0 , 2) ; (1 , 2) ; (2 , 2) ; (1 , 2) ; (0 , 2) ; (0 , 1) ; (1 , 1) ; (0 , 1) ; (0 , 0) ; (1 , 0) ; (2 , 0) ; (1 , 0) ; (0 , 0) ; (0 , 0) ]  *)
let rec coverage ((x, y) : int * int) (lis : int list) : (int * int) list =
  let rec coordinate ((x, y) : int * int) (d : int) : int * int =
    match d with
    | 0 -> (x, y)
    | 1 -> (x, y)
    | 2 -> (x, y + 1)
    | 3 -> (x + 1, y)
    | 4 -> (x, y - 1)
    | 5 -> (x - 1, y)
    | _ -> (x, y)
  in
  match lis with
  | [] -> [(x, y)]
  | h :: t -> (x, y) :: coverage (coordinate (x, y) h) t


(* Task Seven  Compress *)

(** [compress] compresses a program by intaking a program (list of ints) and  outputting a list of tuples
    (m,n) where m represents the instruction, and n represents the time that instruction is repeated before the next instruction
    
    Example:
    # compress letter_e ;;
 - : ( int * int ) list =
[(0 , 1) ; (2 , 2) ; (3 , 2) ; (5 , 2) ; (4 , 1) ; (3 , 1) ; (5 , 1) ; (4 , 1) ; (3 , 2)   ;(5 , 2) ; (1 , 1) ]                *)

let rec compress (l : int list) : (int * int) list =
  let rec compress_help (l : int list) ((x, y) : int * int) : (int * int) list =
    match l with
    | [] -> [(x, y + 1)]
    | h :: t ->
      if h = x then
        compress_help t (h, y + 1)
      else
        (x, y + 1) :: compress_help t (h, 0)
  in
  match l with
  | [] -> []
  | h :: t -> compress_help t (h, 0)



(*Task Eight  Uncompress *)

(** [uncompress] uncompresses a compressed program (See compressed program).
    
Example:
# uncompress ( compress letter_e ) ;;
2 - : int list = [0; 2; 2; 3; 3; 5; 5; 4; 3; 5; 4; 3; 3; 5; 5; 1]       *)

let rec uncompress (l : (int * int) list) : int list =
  let rec uncompress_help ((fst, snd) : int * int) : int list =
    (* Its the fst, snd times to uncompress *)
    match (fst, snd) with 
    | (_, 0) -> []
    | (_, _) -> fst :: uncompress_help (fst, (snd - 1))
  in
  match l with
  | [] -> []
  | h :: t -> uncompress_help h @ uncompress t


(*Task Nine  Optimize *)

(** [optimize] optimizes a program (type program seen above) by eliminating any redunant pen up
or pen down instructions in the program int list.

Precondition: You may assume the pen is always in the up position

Examples
# optimize [1];;
2 - : int list = []
3 # optimize [1;1;1;1];;
4 - : int list = []
5 # optimize [1;1;1;1;0];;
6 - : int list = [0]
7 # optimize [ 1 ; 1 ; 1 ; 1 ; 0 ; 1 ; 0 ; 1 ] ; ;
8 - : int list = [0; 1; 0; 1]
9 # optimize [ 1 ; 1 ; 1 ; 1 ; 0 ; 1 ; 0 ; 1 ; 1 ; 1 ; 1 ] ; ;
10 - : int list = [0; 1; 0; 1]
11 # optimize [0;1;0;1];;
12 - : int list = [0; 1; 0; 1]
13 # optimize [2;3;4;5];;
14 - : int list = [2; 3; 4; 5]
*)

let rec optimize_tail : int list -> int list = function
  | 1 :: 1 :: t -> optimize_tail (1 :: t) (* Skip consecutive 1s *)
  | h :: t -> h :: optimize_tail t (* Keep the current element and process the rest *)
  | [] -> []

let rec optimize : program -> program = function
  | 1 :: t -> optimize t (* Skip the initial 1s *)
  | h :: t -> h :: optimize_tail t (* Once a non-1 is found, keep the rest of the list *)
  | [] -> []







