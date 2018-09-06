open Thwack.Option
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

let math_operators = [
  ("+", "core_common.add");
  ("-", "core_common.sub");
  ("*", "core_common.mlt");
  ("/", "core_common.div");
  ("%", "core_common.mod");
]

let wrapped_operators = math_operators

let infix_operators = List.map fst wrapped_operators

let math_fntype = C.Type.Fn ([C.Type.Num; C.Type.Num], C.Type.Num)
let common_module =
  let name = "common"
  and generic_vars = [
    ("true", C.Type.Bool);
    ("false", C.Type.Bool);
    ("print", C.Type.Fn ([C.Type.Str], C.Type.Unit))
  ] in
  let math_vars = List.map (fun (n, _) -> (n, math_fntype)) math_operators in
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

let pervasive_name = C.Module.qual_name pervasive.modul

let stdlib = C.Symbol_table.with_pervasive pervasive

let core = modules

let has_common_name qual_name =
  (C.Module.qual_name common_module) = qual_name

let is_infix_op qual_name var_name =
  let name = C.Module.Var.Name.to_string var_name in
  has_common_name qual_name && List.exists ((=) name) infix_operators

let is_wrapped_op op =
  List.exists ((=) op) infix_operators

let wrapped_op op =
  let cmp_fn (o1, _) = o1 = op in
  let maybe_op = List.find_opt cmp_fn wrapped_operators in
  maybe_op >>= fun m -> return (snd m)
