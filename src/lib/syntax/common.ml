module Varmap = Map.Make(String)

type var = string

type arith =
  | AAdd
  | AMult
  | ASub
  | ADiv
  | AMod

type cmp =
  | CEq
  | CNeq
  | CLt
  | CGt

type barith =
  | BConj
  | BDisj

type hdl_anno =
  | HDef
  | HExc
  | HHdl1 (* Singleshot *)
  | HHdls (* Multishot*)

type type_expr = var

type typedef = {
  type_name : var;
  type_cons : (var * type_expr list) list
}

type pattern =
  | PTypecon of var * var list

type funcAnno =
  | CANoneLocalComeFrom (* abortive or general handled body. Control can come back to caller non-locally *)
  | CANoneLocalGoto (* general handler. Control goes to a different stack. *)
  | CANone

type label_index =
  | LabelInfty
  | LabelIndex of int

and cap_index = 
  | CapInfty
  | CapIndex of int

and capability = cap_index option * (label_index * int) list

and metadata = capability * (capability list) * label_index list