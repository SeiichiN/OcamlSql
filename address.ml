(**
   アドレス帳
   
   $Id: address.ml,v 0.1 2019/05/21 Seiichi Nukayama $
   Copyright (c) 2019 Seiichi Nukayama, All Rights Reserved.
*)

open Mysql

#use "type.ml"
#use "readfile.ml"
#use "listAll.ml"
#use "disp.ml"

let conf = read_conf "address.conf"

let rec assoc a = function
    [] -> ""
    | (a', b) :: rest ->
            if a = a' then b
            else assoc a rest

let tablename = assoc "tablename" conf
and dbname =  assoc "dbname" conf
and username =  assoc "username" conf
and passwd = assoc "password" conf
and hostname = assoc "hostname" conf;
       
#use "syori.ml"
#use "inputData.ml"

let db = quick_connect ~database:dbname ~password:passwd ~user:username ~host:hostname ()


let syori_tuika () =
    let oneAddress = input_data() in
    let st = "( firstname, lastname, sei, namae, email, memo )" in
    let ml2values m = values [
        ml2str m.firstname;
            ml2str m.lastname;
            ml2str m.sei;
            ml2str m.namae;
            ml2str m.email;
            ml2str m.memo] in
    let insert values = 
        "insert into " ^ tablename ^ st ^  " values " ^ values in
    ignore (exec db (insert (ml2values oneAddress)))

let syori_ichiran () =
    let sql = "select * from " ^ tablename in
    let allData = listAll db sql in
    let addressList = mkRecord allData in
    ignore (disp_address_list addressList)


let get_user_input n mes =
    print_string mes
    ; flush stdout
    ; let user_string = input_line stdin in
    user_string
   

let syori_etsuran () =
    let num = etsuran () in   (* 検索・訂正メニューからユーザーの選択した番号を得る *)
    if num > 0
    then
        let fieldname = (assoc num field_list) in
        let message = (assoc num select_list) in
        let user_str = (get_user_input num message) in
        let sql = "select * from " ^ tablename ^ " where " ^ fieldname ^ " like '%" ^ user_str ^ "%'" in
        let allData = listAll db sql in  (* sqlを実行 データのリストを得る *)
        let aList = mkRecord allData in   (* レコード型のリストに変換する *)
        if (List.length aList) > 1 then ignore (disp_select_list aList);
        let addressList = reselect aList in  (* リストが2つ以上あれば、１つに絞る *)
        ignore (disp_select_list addressList);
        let num2 = edit () in
        if num2 > 0
        then
            let (id', fieldname', newData) = retouch_data num2 addressList in
            let sql = "update " ^ tablename ^ " set " ^ fieldname' ^ " = " ^ (ml2str newData) ^ " where id = " ^ (string_of_int id') in
            ignore (exec db sql)

let syori_delete () =
    let delete_id = string_of_int (select_delete_id()) in
    let sql = "delete from " ^ tablename ^ " where id = " ^ delete_id in
    ignore (exec db sql);
    print_endline ("id= " ^ delete_id ^ " のデータを削除しました。")

let _ =
    let no = ref 9 in
    while (!no > 0) do
        no := menu ();
        match !no with
        1 -> syori_tuika ()
        | 2 -> syori_etsuran ()
        | 3 -> syori_ichiran ()
        | 4 -> syori_delete ()
        | _ ->
            print_endline "bye"
    done


(* Mysql.mapを少し改造 *)
    (*
let map db sql build =
  let r = Mysql.exec db sql in
  let col = Mysql.column r in
  let rec loop l =
    match Mysql.fetch r with
      None -> 
	l
    | Some x -> 
	loop (build (col ~row:x) :: l)
  in
  loop []
*)

(* mapがあるならfoldも *)
(*
let fold db sql f init =
  let r = Mysql.exec db sql in
  let col = Mysql.column r in
  let rec loop l =
    match Mysql.fetch r with
      None -> 
	l
    | Some x -> 
	loop (f l (col ~row:x))
  in
  loop init
  *)
