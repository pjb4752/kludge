module C = Chunkee

let name =
  let root = C.Mod_name.Name.from_string "core" in
  let path = C.Mod_name.Path.from_list [root] in
  let name = C.Mod_name.Name.from_string "list" in
  C.Mod_name.make path name

let functions = [
  Lua_var.make_fn "empty" (C.Type.Fn([], C.Type.List));
  Lua_var.make_fn "cons" (C.Type.Fn ([C.Type.List; C.Type.Any], C.Type.List));
  Lua_var.make_fn "empty?" (C.Type.Fn ([C.Type.List], C.Type.Bool));
  Lua_var.make_fn "head" (C.Type.Fn ([C.Type.List], C.Type.Any));
  Lua_var.make_fn "tail" (C.Type.Fn ([C.Type.List], C.Type.List));
  Lua_var.make_fn "nth" (C.Type.Fn ([C.Type.List; C.Type.Num], C.Type.Any));
]

let vars = functions
