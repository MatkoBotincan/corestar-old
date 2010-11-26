(********************************************************
   This file is part of jStar
        src/utils/misc.ml
   Release
        $Release$
   Version
        $Rev$
   $Copyright$

   jStar is distributed under a BSD license,  see,
      LICENSE.txt
 ********************************************************)

open Backtrack 

let map_option f l =
  let f' acc x = match f x with
    | None -> acc
    | Some y -> y :: acc in
  List.rev (List.fold_left f' [] l)

let map_lift_exists f l
    =
  List.fold_right 
    (fun el rest-> 
      match f el, rest  with
	None,_ -> rest
   |	Some el,Some rest -> Some (el::rest)
   |  Some el, None -> Some [el]) l None

let map_lift_forall f l
    =
  List.fold_right 
    (fun el rest-> 
      match f el, rest  with
     	  None,_ -> None
	    | _, None -> None
      |	Some el,Some rest -> Some (el::rest)) l (Some [])


type ('a,'b) sum = 
    Inr of 'a
  | Inl of 'b

let map_sum f l
    =
  List.fold_right
    (fun el (restl,restr) -> 
      match f el with
      | Inl l -> (l::restl,restr)
      | Inr r -> (restl,r::restr)) l ([],[])


let remove_duplicates c l =
  let l = List.sort c l in 
  snd (
  List.fold_left 
    (fun (complast,list) next  ->
      if complast next = 0 then (complast,list) else (c next, next::list)
	) ((fun _ -> -1),[]) l
    )


(* TODO(rgrig): Isn't intcmp x y = compare y x? *)
let intcmp a b =
  if a<b then -1 else if a=b then 0 else 1

(* TODO(rgrig): Isn't intcmp2 x y = compare y x? *)
let intcmp2 (x1,x2) (y1,y2) =
  let v = intcmp x1 y1 in 
  if v = 0 then intcmp x2 y2 
  else v

let rec map_and_find f = function
  | [] -> raise Not_found
  | x :: xs -> try f x with _ -> map_and_find f xs

let rec find_no_match_simp f l =
  let rec fnm_inner f l =
  match l with 
    [] -> raise No_match
  | x::l -> try f x with No_match -> fnm_inner f l
  in fnm_inner f l 



let lift_pair f = 
  fun (x,y) -> (f x, f y)

let lift_option f =
  fun x -> match x with
    Some x -> f x
  | None -> None

let rec add_index 
    ( xs : 'a list ) 
    ( i : int ) : ('a * int) list = 
  match xs with  | []     ->  [] 
                 | y::ys  ->  ( (y,i) :: (add_index ys (i+1)) ) 

let memo2 f =
  let cache = Hashtbl.create 101 in
  fun x y ->
    try Hashtbl.find cache (x, y)
    with Not_found ->
      let r = f x y in
      (Hashtbl.add cache (x, y) r; r)

let cross_product l1 l2 =
  let product l v2 = List.map (fun v1 -> (v1, v2)) l in
  List.concat (List.map (product l1) l2)
