module C = Chunkee

type t

val make_const: string -> C.Type.t -> t

val make_fn: string -> C.Type.t -> t

val make_simple_op: string -> string -> C.Type.t -> t

val make_mapped_op: string -> string -> string -> C.Type.t -> t

val name: t -> string

val compiler_name: t -> string

val tipe: t -> C.Type.t

val compiler_operator: t -> string -> Lua_operator.t option
