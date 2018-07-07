type t

val make_stmt: string -> string -> t

val make_expr: string -> t

val insert_preamble: t -> t -> t

val is_stmt: t -> bool

val is_expr: t -> bool

val preamble_stmt: t -> string

val result_expr: t -> string

val to_result_string: ?target:string -> t -> string
