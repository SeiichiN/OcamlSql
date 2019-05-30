(*
 * Test for Mysql.Prepared module
 *)

open Printf
module P = Mysql.Prepared

let s = String.copy

(* 接続 *)
let db = Mysql.quick_connect ~database:(s "test") ~user:(s "root") ~host:(s "bgn-003") ()


(* テーブル作成 *)
let (_:Mysql.result) = Mysql.exec db (s "CREATE TABLE IF NOT EXISTS test (id INT, v VARCHAR(10)) ENGINE=MEMORY")

(* データの挿入 *)
let () =
  (* Mysql.Prepared の create メソッドを使うみたい *)
  let insert = P.create db (s "INSERT INTO test VALUES (?,?)") in
  for i = 10 to 15 do
    (* Mysql.Prepared の execute メソッドで、insert に配列を渡す *)
    ignore (P.execute insert [|string_of_int i; sprintf "value %d" i|])
  done;
  for i = 16 to 20 do
    (* Mysql.Prepared の execute_null メソッドで、nullデータを送ることができる *)
    ignore (P.execute_null insert [|Some (string_of_int i); None|])
  done;
  (* insertオブジェクト(?)を閉じる *)
  P.close insert

let rec loop t =
  match P.fetch t with
  | Some arr -> Array.iter (function Some s -> printf "%s " s
                                        | None -> print_string "<NULL>") arr;
    print_endline "";
    loop t
  | None -> ()

let () =
  let select = P.create db (s "SELECT * FROM test WHERE id > ?") in
  print_endline "> 13";
  loop (P.execute select [|s "13"|]);
  print_endline "> 19";
  loop (P.execute select [|s "19"|]);
  print_endline "> 20";
  loop (P.execute select [|s "20"|]);
  P.close select;
  print_endline "done all";
  ()

let (_:Mysql.result) = Mysql.exec db (s "DROP TABLE test")


(* 接続解除 *)
let () = Mysql.disconnect db

