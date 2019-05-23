(**
   アドレス帳
   
   $Id: address.ml,v 0.1 2019/05/21 Seiichi Nukayama $
   Copyright (c) 2019 Seiichi Nukayama, All Rights Reserved.
*)

open Mysql

#use "readfile.ml"
#use "listAll.ml"
#use "menu.ml"
#use "type.ml"
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
       
#use "inputData.ml"

let db = quick_connect ~database:dbname ~password:passwd ~user:username ~host:hostname ()


(* Mysql.mapを少し改造 *)
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

(* mapがあるならfoldも *)
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
 
let _ =
    let no = menu () in
    match no with
    1 -> 
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
        ignore (exec db (insert (ml2values oneAddress)));
        "data In!"
    | 4 ->
        let sql = "select * from " ^ tablename in
        let allData = listAll db sql in
        let addressList = mkRecord allData in
        disp_address_list addressList
    | _ ->
        "bye"
