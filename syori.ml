(*
 * syori.ml
 *)

let syori () =
    let no = menu () in
    if no = 4
    then
        let sql = "select * from " ^ tablename in
        listAll db sql
