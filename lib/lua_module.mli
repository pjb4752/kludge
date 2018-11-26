module C = Chunkee

type compiler_module_t = Chunkee.Module.t

type t

val make: C.Mod_name.t -> Lua_var.t list -> t

val name: t -> C.Mod_name.t

val find_operator_name: t -> string -> string option

val operator_exists: t -> string -> bool

val to_compiler_module: t -> compiler_module_t
