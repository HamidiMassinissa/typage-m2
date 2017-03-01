(* recursive types *)

type ty =
  | Tybase of type_identifier
  | Tyvar of type_variable
  | Tybind of type_variable * ty
  | Typroduct of ty * ty
  | Tyarrow of ty * ty

 and type_identifier = string

 and type_variable = string

 and t = ty

