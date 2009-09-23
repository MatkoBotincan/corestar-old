(**************************************************************************)
(*                                                                        *)
(*     The Alt-ergo theorem prover                                        *)
(*     Copyright (C) 2006-2009                                            *)
(*                                                                        *)
(*     Sylvain Conchon                                                    *)
(*     Evelyne Contejean                                                  *)
(*     Stephane Lescuyer                                                  *)
(*     Mohamed Iguernelala                                                *)
(*                                                                        *)
(*     CNRS - INRIA - Universite Paris Sud                                *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C licence     *)
(*                                                                        *)
(**************************************************************************)

open Hashcons
open Format

type t = 
    | Tint
    | Trat
    | Tbool
    | Tvar of tvar
    | Tbitv of int
    | Text of t list * Hstring.t
and tvar = { v : int ; mutable value : t option }

exception TypeClash of t*t
exception Shorten of t

(* smart constructors *)

let tunit = Text([],Hstring.make "unit")
let text l s = Text(l,Hstring.make s)

let rec shorten t = 
  match t with
  | Tvar {value=None}  -> t
  | Tvar {value=Some(Tvar{value=None} as t')} -> t'
  | Tvar ({value=Some(Tvar t2)} as t1) -> t1.value <- t2.value; shorten t
  | Tvar {value=Some t'} -> shorten t'
  | Text (l,s) -> Text(List.map shorten l,s)
  | _ -> t

let fresh_var = 
  let cpt = ref (-1) in
  fun () -> incr cpt; {v= !cpt ; value = None }

let fresh_empty_text =
  let cpt = ref (-1) in
  fun () -> incr cpt; text [] ("'_c"^(string_of_int !cpt))

let rec hash t = match t with
  | Tvar{v=v} -> v
  | Text(l,s) -> abs (List.fold_left (fun acc x-> acc*19 + hash x) s.tag l)
  | _ -> Hashtbl.hash t

let rec equal t1 t2 = match shorten t1 , shorten t2 with
    Tvar{v=v1} , Tvar{v=v2} -> v1 = v2
  | Text(l1,s1) , Text(l2,s2) ->
      (try s1.tag = s2.tag && List.for_all2 equal l1 l2
       with Invalid_argument _ -> false)
  | t1 , t2 -> t1 = t2


let occurs {v=n} = 
  let rec occursrec = function
      Tvar {v=m} -> n=m
    | Text(l,_) -> List.exists occursrec l
    | _ -> false
  in occursrec

(*** destructive unification ***)
let rec unify t1 t2 = 
  let t1 = shorten t1 in
  let t2 = shorten t2 in
  match t1 , t2 with
      Tvar ({v=n;value=None} as tv1), Tvar {v=m;value=None} ->
	if n<>m then tv1.value <- Some t2
    | _ ,  Tvar ({value=None} as tv) -> 
	if (occurs tv t1) then raise (TypeClash(t1,t2));
	tv.value <- Some t1
    | Tvar ({value=None} as tv) , _ -> 
	  if (occurs tv t2) then raise (TypeClash(t1,t2));
	tv.value <- Some t2
    | Text(l1,s1) , Text(l2,s2) when Hstring.equal s1 s2 ->
	List.iter2 unify l1 l2
    | Tint , Tint | Tbool , Tbool | Trat , Trat -> ()
    | Tbitv n , Tbitv m when m=n -> ()
    | _ , _ -> raise (TypeClash(t1,t2))


(*** matching with a substitution mechanism ***)
module M = Map.Make(struct type t=int let compare = compare end)
type subst = t M.t

let esubst = M.empty

let rec matching s pat t = 
  match pat , t with
    | Tvar {v=n;value=None} , _ -> 
	(try if not (equal (M.find n s) t) then raise (TypeClash(pat,t)); s
	 with Not_found -> M.add n t s)
    | Tvar {value=_} , _ -> raise (Shorten pat)
    | Text (l1,s1) , Text (l2,s2) when Hstring.equal s1 s2 ->
	List.fold_left2 matching s l1 l2 
    | Tint , Tint | Tbool , Tbool | Trat , Trat -> s
    | Tbitv n , Tbitv m when n=m -> s
    | _ , _ -> raise (TypeClash(pat,t))

let rec apply_subst s = function
  | Tvar {v=n} as t -> (try M.find n s with Not_found -> t)
  | Text (l,e) -> Text(List.map (apply_subst s) l,e)
  | t -> t

let union_subst s1 s2 = 
  M.fold (fun k x s2 -> M.add k x s2) (M.map (apply_subst s2)  s1) s2

let compare_subst = M.compare compare

(*** pretty print ***)
let rec print fmt = function
  | Tint -> fprintf fmt "int"
  | Trat -> fprintf fmt "real"
  | Tbool -> fprintf fmt "bool"
  | Tbitv n -> fprintf fmt "bitv[%d]" n
  | Tvar{v=v ; value = None} -> fprintf fmt "'a_%d" v
  | Tvar{v=v ; value = Some t} -> fprintf fmt "('a_%d->%a)" v print t
  | Text(l,s) -> fprintf fmt "%a%s" printl l (Hstring.view s)

and printl fmt = function
    [] -> ()
  | [t] -> fprintf fmt "%a " print t
  | t::l -> fprintf fmt "%a,%a" print t printl l
    
