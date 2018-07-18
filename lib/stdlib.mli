module C = Chunkee

val pervasive: C.Pervasive.t

val stdlib: C.Symbol_table.t

val is_infix_op: C.Module.Qual_name.t -> C.Module.Var.Name.t -> bool
