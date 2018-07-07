val emit_node: (Chunkee.Name.t Chunkee.Node.t) -> Lua_stmt.t

val emit_typed_node: Chunkee.Typecheck.t -> string

val emit: Chunkee.Typecheck.t list -> string list
