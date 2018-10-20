module C = Chunkee

val pervasive: C.Pervasive.t

val pervasive_name: C.Mod_name.t

(*val stdlib: C.Symbol_table.t*)

val is_infix_op: C.Mod_name.t -> C.Var.Name.t -> bool

val is_wrapped_op: string -> bool

val wrapped_op: string -> string option
