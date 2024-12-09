open Parser_plaf.Ast
open Parser_plaf.Parser

open Ds

(** [eval_expr e] evaluates expression [e] *)
let rec eval_expr : expr -> exp_val ea_result =
  fun e ->
  match e with
  | Int(n) ->
    return (NumVal n)
  | Var(id) ->
    apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1+n2))
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1-n2))
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1*n2))
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return (NumVal (n1/n2))
  | Let(id,def,body) ->
    eval_expr def >>= 
    extend_env id >>+
    eval_expr body 
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b 
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return (BoolVal (n = 0))
  | Pair(e1,e2) ->
    eval_expr e1 >>= fun ev1 ->
    eval_expr e2 >>= fun ev2 ->
    return (PairVal(ev1,ev2))
  | Fst(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun (l,_) ->
    return l
  | Snd(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun (_,r) ->
    return r
  | Debug(_e) ->
    string_of_env >>= fun str ->
    print_endline str; 
    error "Debug called"


  (*Extensions to LET - HW3 *)  
  (*Binary Trees*)

  (*Empty Tree, return an empty TreeVal*)
  | EmptyTree (_t) -> 
    return (TreeVal Empty)
  (*Node, takes e1,e2,e3 creates a new tree with data e1 with left and right subtrees e2 and e3*)
  | Node(e1,e2,e3) ->
    eval_expr e1 >>= fun d ->
    eval_expr e2 >>= is_TreeVal >>= fun l ->
    eval_expr e3 >>= is_TreeVal >>= fun r ->
    return (TreeVal (Node(d, l, r)))
(*IsEmpty*)
  | IsEmpty(e) ->
    eval_expr e >>= is_TreeVal >>= fun t ->
    (match t with
    | Empty  -> return (BoolVal true)
    | Node _ -> return (BoolVal false))
  
  (*CaseT*)
  (*Related to match*)
 | CaseT (e1,e2,id1,id2,id3,e3) -> 
    eval_expr e1 >>= is_TreeVal >>= (function 
    |  Empty -> eval_expr e2
    |  (Node(v, l, r)) ->
        extend_env id1 v >>+
        extend_env id2 (TreeVal l) >>+
        extend_env id3 (TreeVal r) >>+
        eval_expr e3 )

  (*Record*)
  | Record fs ->
    if check_duplicates fs then
      error "Record: duplicate fields"
    else
      let ids, exprs_with_bools = List.split fs in
      let exprs = List.map snd exprs_with_bools in
      eval_exprs exprs >>= fun values ->
      let fields = List.combine ids (List.combine (List.map fst exprs_with_bools) values) in
      return (RecordVal fields)
| Proj (e, id) ->
    eval_expr e >>= fun ev ->
    (match ev with
    | RecordVal fields ->
        let rec find_field id fields =
          match fields with
          | [] -> None
          | (key, (_, value))::rest ->
              if key = id then Some value else find_field id rest
        in
        (match find_field id fields with
         | Some value -> return value
         | None -> error "Proj: field does not exist")
    | _ -> error "Expected a record")

  


  (*END*)
  | _ -> failwith "Not implemented yet!"
  
  and eval_exprs : expr list -> (exp_val list) ea_result =
    fun es ->
    match es with
    | [] -> return []
    | h :: t ->
      eval_expr h >>= fun i ->
      eval_exprs t >>= fun l ->
      return (i :: l)




(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e


(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_prog
  in run c
  


