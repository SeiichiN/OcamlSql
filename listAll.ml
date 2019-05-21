(* listAll.ml *)


(* 1件のレコードをタプルにして、全レコードをタプルのリストにする *)
let listAll db sql =
  let r = Mysql.exec db sql in
  let col = Mysql.column r in
  let row x = (
    not_null int2ml (col ~key:"id" ~row:x)
  , not_null str2ml (col ~key:"firstname" ~row:x)
  , not_null str2ml (col ~key:"lastname" ~row:x)
  , not_null str2ml (col ~key:"sei" ~row:x)
  , not_null str2ml (col ~key:"namae" ~row:x)
  , not_null str2ml (col ~key:"email" ~row:x)
  , not_null str2ml (col ~key:"memo" ~row:x)
  ) in
  let rec loop r =
    match Mysql.fetch r with
      None -> []
    | Some x -> row x :: loop r (* Some (col ~key:"firstname" ~row:x) *)
  in
  loop r
