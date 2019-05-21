(**
   アドレス帳
   
   $Id: address.ml,v 0.1 2019/05/21 Seiichi Nukayama $
   Copyright (c) 2019 Seiichi Nukayama, All Rights Reserved.
*)

open Mysql

#use "readfile.ml"
#use "listAll.ml"

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
and hostname = assoc "hostname" conf
       
let db = quick_connect ~database:dbname ~password:passwd ~user:username ~host:hostname ()

let sql = "select * from " ^ tablename


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
    listAll db sql


