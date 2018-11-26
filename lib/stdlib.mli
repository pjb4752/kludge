module C = Chunkee

val find_lua_module: C.Mod_name.t -> Lua_module.t option

val pervasive: C.Pervasive.t

val compiler_modules: C.Module_tree.t
