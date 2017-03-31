open RecAST

module TypingGraph : sig
  type t = tygraph

   and tygraph = node

   and node = {
       label: constructor;
       succ: node list ref;
       equivalent: node option ref;
     }

   and constructor =
     | VarCons
     | BaseCons
     | ArrowCons
     | ProductCons


  val from_tyscheme: tyscheme -> t
  val to_string: t -> string
end = struct
  type t = tygraph

   and tygraph = node

   and node = {
       label: constructor;
       succ: node list ref;
       equivalent: node option ref;
     }

   and constructor =
     | VarCons
     | BaseCons
     | ArrowCons
     | ProductCons

  let mk_node ?(succ=ref []) ?(equivalent=ref None) label = {
      label; succ; equivalent 
    }

  let from_tyscheme tysch =
    let rec aux tysch binders a_top =
      match tysch with
      | TVar tv ->
         if (List.mem tv binders)
         then mk_node VarCons ~succ:(ref [a_top])
         else mk_node VarCons

      | TBase tid -> a_top

      | TArrow (ity,oty) ->
         let a_node =
           mk_node ArrowCons
         in
         let ity_node = aux ity binders a_top in
         let oty_node = aux oty binders a_top in
         a_node.succ:=(ity_node::oty_node::[]);
         a_node
         
      | TProduct (aty,bty) -> a_top

      | TRec (binders,ty) ->
         aux ty binders a_top
    in
    aux tysch [] (mk_node ArrowCons)

  let to_string tg = ""
end

let ty = TRec (["x"], TArrow (TVar "x",TVar "x"))
let x = TypingGraph.from_tyscheme ty
