(**
   バグとラックシステム サンプルコード
   DB操作の基盤
   
   $Id: db.ml,v 1.1 2007/05/23 04:27:00 ogasawara Exp $
*)
(*
   Copyright (c) 2007 ogasawara@itpl.co.jp All Rights Reserved.
   
   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:
  
   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,   
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)
open Mysql

(* 接続先のユーザ名とデータベースを決めておく *)
(* let connect () = quick_connect ~database:"p_memo" ~password:"0405" ~user:"billie" () *)
let db = quick_connect ~database:"address" ~password:"ykk" ~user:"ykk" ()

(* let db = connect () *)

let sql = "select * from list"

(* 最初に一行のデータが欲しいとき *)
let first db sql =
  let r = Mysql.exec db sql in
  let col = Mysql.column r in
  let row x = (col ~key:"namae" ~row:x) in
  let rec loop r =
  match Mysql.fetch r with
    None -> []
  | Some x -> row x :: loop r (* Some (col ~key:"firstname" ~row:x) *)
  in
  loop r

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
    first db sql

