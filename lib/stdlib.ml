module C = Chunkee

let lua_common = Core_common.(Lua_module.make name vars)

let lua_list = Core_list.(Lua_module.make name vars)

let lua_modules = [
  lua_common;
  lua_list;
]

let find_lua_module name =
  List.find_opt (fun modul -> (Lua_module.name modul) = name) lua_modules

let pervasive_module = Lua_module.to_compiler_module lua_common

let pervasive = { C.Pervasive.modul = pervasive_module }

let compiler_modules =
  List.fold_left (fun tree modul ->
    Lua_module.to_compiler_module modul |> C.Module_tree.insert_module tree
  ) C.Module_tree.empty lua_modules

(*let is_infix_op qual_name var_name =*)
  (*let name = C.Var.Name.to_string var_name in*)
  (*has_pervasive_name qual_name && List.mem name*)

(*let is_wrapped_op op =*)
  (*List.exists ((=) op) infix_operators*)

(*let wrapped_op op =*)
  (*let cmp_fn (o1, _) = o1 = op in*)
  (*let maybe_op = List.find_opt cmp_fn wrapped_operators in*)
  (*maybe_op >>= fun m -> return (snd m)*)
