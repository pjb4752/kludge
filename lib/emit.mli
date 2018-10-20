module C = Chunkee
module Node = C.Ast.Resolved_node

val emit_require: C.Mod_name.t -> string

val emit_node: Node.t -> Lua_stmt.t

val emit_typed_node: (Node.t * C.Type.t) -> string

val emit: (Node.t * C.Type.t) list -> string list
