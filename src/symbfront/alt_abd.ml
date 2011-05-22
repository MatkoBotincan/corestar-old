(** Entry point for experiments on abduction with procedure calls. *)

open Format
module C = Core
module G = Cfg

let map_proc_body f x = { x with C.proc_body = f x.C.proc_body }

let parse fn =
  System.parse_file Parser.symb_question_file Lexer.token fn "core"

(* helpers for [mk_intermediate_cfg] {{{ *)
let mic_create_vertices g cs =
  let succ = Hashtbl.create 13 in
  let vs = List.map G.CfgH.V.create cs in
  List.iter (G.CfgH.add_vertex g) vs;
  Misc.iter_pairs (Hashtbl.add succ) vs;
  succ

let mic_hash_labels g =
  let labels = Hashtbl.create 13 in
  let f v = match G.CfgH.V.label v with
    | C.Label_stmt_core l -> Hashtbl.add labels l v
    | _ -> () in
  G.CfgH.iter_vertex f g;
  labels

let mic_add_edges r labels succ =
  let g = r.G.ProcedureH.cfg in
  let get_succ x =
    try Hashtbl.find succ x with Not_found -> r.G.ProcedureH.stop in
  let vertex_of_label l =
    try Hashtbl.find labels l
    with Not_found -> failwith "bad cfg (todo: nice user error)" in
  let add_outgoing x = match G.CfgH.V.label x with
    | C.Goto_stmt_core ls ->
        List.iter (fun l -> G.CfgH.add_edge g x (vertex_of_label l)) ls
    | C.End -> G.CfgH.add_edge g x r.G.ProcedureH.stop
    | _ -> G.CfgH.add_edge g x (get_succ x) in
  G.CfgH.iter_vertex add_outgoing g

(* }}} *)

let mk_intermediate_cfg cs =
  let g = G.CfgH.create () in
  let start = G.CfgH.V.create C.Nop_stmt_core in
  let stop = G.CfgH.V.create C.Nop_stmt_core in
  let succ = mic_create_vertices g cs in
  let labels = mic_hash_labels g in
  let r =
    { G.ProcedureH.cfg = g
    ; G.ProcedureH.start = start
    ; G.ProcedureH.stop = stop } in
  mic_add_edges r labels succ;
  r

(* TODO(rgrig): This fails for 1->2, 1->3, 2->4, 3->4, all interesting. *)
let simplify_cfg g =
  let sg = G.Cfg.create () in
  let node_stack = Stack.create () in
  Stack.push None node_stack;
  let push_rep rv =
    G.Cfg.add_vertex sg rv;
    match Stack.top node_stack with
	Some v -> G.Cfg.add_edge sg v rv
      | None -> ();
    Stack.push (Some rv) node_stack in
  let pop_rep () = ignore (Stack.pop node_stack) in
  let pre v = match G.CfgH.V.label v with
      C.Assignment_core call -> 
	push_rep (G.Cfg.V.create (G.Assign_cfg call))
    | C.Call_core (fname, call) ->
	push_rep (G.Cfg.V.create (G.Call_cfg (fname, call))) 
    | _ -> () in
  let post v = match G.CfgH.V.label v with
      C.Assignment_core _
    | C.Call_core _ ->
	pop_rep ()
    | _ -> () in
  G.Dfs.iter ~pre:pre ~post:post g;
  sg

let mk_cfg cs = 
  let g = mk_intermediate_cfg cs in
  simplify_cfg g.G.ProcedureH.cfg

let interpret gs =
  let f { C.proc_name=n; C.proc_spec=_; C.proc_body=_ } =
    eprintf "@[int %s@." n in
  List.iter f gs
(*   failwith "todo" *)

let output_Cfg { C.proc_name=n; C.proc_spec=_; C.proc_body=g } =
  G.output_Cfg (n ^ "_Cfg.dot") g

let output_CfgH { C.proc_name=n; C.proc_spec=_; C.proc_body=g } =
  G.output_CfgH (n ^ "_CfgH.dot") g.G.ProcedureH.cfg

let main f =
  let ps = parse f in
  let igs = List.map (map_proc_body mk_intermediate_cfg) ps in
  List.iter output_CfgH igs;
  let gs = List.map (map_proc_body mk_cfg) ps in
  List.iter output_Cfg gs;
  interpret gs

let _ =
  Arg.parse [] main "alt_abd <file>";
