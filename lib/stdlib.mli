module C = Chunkee

val common_module: C.Module.t

val is_infix_op: C.Module.Qual_name.t -> C.Module.Var.Name.t -> bool
