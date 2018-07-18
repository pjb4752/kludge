module C = Chunkee

let make_var modul (name, t) =
  let name = C.Module.Var.Name.from_string name in
  C.Module.define_var modul name t

let make_module root name vars =
  let name = C.Module.Name.from_string name in
  let path = C.Module.Path.from_list [root] in
  let mempty = C.Module.from_parts path name in
  List.fold_left make_var mempty vars

let root = C.Module.Name.from_string "core"

let math_operators = ["+"; "-"; "*"; "/"; "%"]

let infix_operators = math_operators

let math_fntype = C.Type.Fn ([C.Type.Num; C.Type.Num], C.Type.Num)
let common_module =
  let name = "common"
  and generic_vars = [
    ("true", C.Type.Bool);
    ("false", C.Type.Bool);
    ("print", C.Type.Fn ([C.Type.Str], C.Type.Unit))
  ] in
  let math_vars = List.map (fun n -> (n, math_fntype)) math_operators in
  let vars = generic_vars @ math_vars in
  make_module root name vars

let list_module =
  let name = "list"
  and vars = [
    ("empty", C.Type.Fn ([], C.Type.List));
    ("cons", C.Type.Fn ([C.Type.List; C.Type.Any], C.Type.List));
    ("empty?", C.Type.Fn ([C.Type.List], C.Type.Bool));
    ("head", C.Type.Fn ([C.Type.List], C.Type.Any));
    ("tail", C.Type.Fn ([C.Type.List], C.Type.List));
    ("nth", C.Type.Fn ([C.Type.List; C.Type.Num], C.Type.Any));
  ] in
  make_module root name vars

let modules = [
  common_module;
  list_module;
]

let pervasive = { C.Pervasive.modul = common_module }

let stdlib = C.Symbol_table.with_pervasive pervasive

let core = modules

let has_common_name qual_name =
  (C.Module.qual_name common_module) = qual_name

let is_infix_op qual_name var_name =
  let name = C.Module.Var.Name.to_string var_name in
  has_common_name qual_name && List.exists ((=) name) infix_operators
