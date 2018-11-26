module C = Chunkee

let name =
  let root = C.Mod_name.Name.from_string "core" in
  let path = C.Mod_name.Path.from_list [root] in
  let name = C.Mod_name.Name.from_string "common" in
  C.Mod_name.make path name

let constants = [
  Lua_var.make_const "true" C.Type.Bool;
  Lua_var.make_const "false" C.Type.Bool;
]

let functions = [
  Lua_var.make_fn "print" (C.Type.Fn ([C.Type.Str], C.Type.Unit))
]

let math_fntype = C.Type.Fn ([C.Type.Num; C.Type.Num], C.Type.Num)
let bool_fntype = C.Type.Fn ([C.Type.Num; C.Type.Num], C.Type.Bool)

let math_operators = [
  Lua_var.make_simple_op "+" "core_common.add" math_fntype;
  Lua_var.make_simple_op "-" "core_common.sub" math_fntype;
  Lua_var.make_simple_op "*" "core_common.mlt" math_fntype;
  Lua_var.make_simple_op "/" "core_common.div" math_fntype;
  Lua_var.make_simple_op "%" "core_common.mod" math_fntype;
]

let bool_operators = [
  Lua_var.make_mapped_op "==" "core_common.eq" "=" bool_fntype;
  Lua_var.make_simple_op ">" "core_common.gtn" bool_fntype;
  Lua_var.make_simple_op ">=" "core_common.gte" bool_fntype;
  Lua_var.make_simple_op "<" "core_common.ltn" bool_fntype;
  Lua_var.make_simple_op "<=" "core_common.lte" bool_fntype;
  Lua_var.make_mapped_op "=~" "core_common.neq" "not=" bool_fntype;
]

let operators = math_operators @ bool_operators

let vars = constants @ functions @ operators
