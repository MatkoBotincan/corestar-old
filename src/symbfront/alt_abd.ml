(** Entry point for experiments on abduction with procedure calls. *)

open Format
module C = Core

type cfg_vertex =
    Assign_cfg of C.call_core
  | Call_cfg of string * C.call_core

module Cfg = Graph.Imperative.Digraph.Abstract 
  (struct type t = cfg_vertex end)
module CfgH = Graph.Imperative.Digraph.Abstract 
  (struct type t = C.core_statement end)

let map_proc_body f x = { x with C.proc_body = f x.C.proc_body }

let parse fn =
  System.parse_file Parser.symb_question_file Lexer.token fn "core"

let mk_intermediate_cfg cs = 
  let g = CfgH.create () in
  let labels = Hashtbl.create 13 in
  let gotos = HashSet.create 13 in
  let miv piv s = (* make intermediate vertex *)
    let civ = CfgH.V.create s in
    CfgH.add_vertex g civ;
    (match piv with
      | Some piv -> CfgH.add_edge g piv civ
      | None -> ());
    match s with
      | C.Goto_stmt_core ls -> HashSet.add gotos (civ, ls); None
      | C.Label_stmt_core l -> Hashtbl.add labels l civ; Some civ
      | _ -> Some civ in
  ignore (List.fold_left miv None cs);
  let ages (v, ls) = (* add goto edges *)
    List.iter (fun l -> CfgH.add_edge g v (Hashtbl.find labels l)) ls in
  HashSet.iter ages gotos;
  g

let simplify_cfg g =
  failwith "todo"

let mk_cfg cs = 
  let g = mk_intermediate_cfg cs in
  simplify_cfg g

let interpret gs =
  let f { C.proc_name=n; C.proc_spec=_; C.proc_body=_ } =
    eprintf "@[int %s@." n in
  List.iter f gs
(*   failwith "todo" *)

let main f =
  let ps = parse f in
  let gs = List.map (map_proc_body mk_cfg) ps in
  interpret gs

let _ =
  Arg.parse [] main "alt_abd <file>";
