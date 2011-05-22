open Format
module C = Core

type cfg_vertex =
    Assign_cfg of C.call_core
  | Call_cfg of string * C.call_core
  | Nop_cfg
  (* NOTE: [Nop_cfg] gives some flexibility in choosing the shape of the graph.
  For example, [Procedure] below assumes one start and one stop node. *)

module Cfg = Graph.Imperative.Digraph.Abstract 
  (struct type t = cfg_vertex end)
module CfgH = Graph.Imperative.Digraph.Abstract 
  (struct type t = C.core_statement end)
module Dfs = Graph.Traverse.Dfs(CfgH)

module MakeProcedure (Cfg : Graph.Sig.I ) = struct
  type t =
    { cfg : Cfg.t
    ; start : Cfg.vertex
    ; stop : Cfg.vertex }
end

module Procedure = MakeProcedure (Cfg)
module ProcedureH = MakeProcedure (CfgH)

module Display_Cfg = struct
  let vertex_name v = match Cfg.V.label v with
      Assign_cfg _ -> "Assign"
    | Call_cfg (fname, _) -> "Call " ^ fname
    | Nop_cfg -> "nop"
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let edge_attributes _ = []
  let get_subgraph _ = None
  include Cfg
end
module Dot_Cfg = Graph.Graphviz.Dot(Display_Cfg)

module Display_CfgH = struct
  let vertex_name v = match CfgH.V.label v with
      C.Nop_stmt_core -> "NOP"
    | C.Label_stmt_core s -> "Label:" ^ s
    | C.Assignment_core _ -> "Assign"
    | C.Call_core (fname, _) -> "Call " ^ fname
    | C.Goto_stmt_core ss -> "Goto:" ^ (String.concat ", " ss)
    | C.End -> "End"
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes v = match CfgH.V.label v with
      C.Assignment_core _
    | C.Call_core _ -> [`Style `Bold]
    | _ -> []
  let default_edge_attributes _ = []
  let edge_attributes _ = []
  let get_subgraph _ = None
  include CfgH
end
module Dot_CfgH = Graph.Graphviz.Dot(Display_CfgH)
  
let fileout file_name f =
  try
    let o = open_out file_name in
      f o; close_out o
  with _ -> eprintf "@[Could not create file %s@." file_name

let output_Cfg file_name g = 
  fileout file_name (fun o -> Dot_Cfg.output_graph o g)

let output_CfgH file_name g =
  fileout file_name (fun o -> Dot_CfgH.output_graph o g)
