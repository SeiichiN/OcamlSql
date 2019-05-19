(*
    $Id: test.ml -- by Seiichi Nukayama 2019.05.19

    Simple demo for the Mysql module.
*)

open Mysql

(* login informations *)

let s = String.copy

let db = quick_connect ~database:"test" ~user:(s "billie") ~password:(s "0405") ()
    
let table       =
                [ ("one"    , 1, 1.0)
                ; ("two"    , 2, 2.0)
                ; ("three"  , 3, 3.0)
                ; (",':-"   , 4, 4.0)
                ]

                (* # mk_table ();; *)
let mk_table () =
    let _r = exec db "create table caml (a char(64), b int, c float)"     in
    db

    (* # fill_table db;; *)
let fill_table c =
    let ml2values (a,b,c) = values [ml2str a; ml2int b; ml2float c]     in
    let insert values     = "insert into caml values " ^ values         in
    let rec loop = function
        | []    -> ()
        | x::xs -> ( ignore (exec c (insert (ml2values x)))
        ; loop xs
        )                                                    
    in
        loop table

        (* # read_table db;; *)
let read_table c =
    let r                   = exec c "select * from caml" in
    let col                 = column r                              in
    let row x               = ( not_null str2ml   (col ~key:"a" ~row:x)
    , not_null int2ml   (col ~key:"b" ~row:x)
    , not_null float2ml (col ~key:"c" ~row:x)
    )                                     in
    let rec loop = function
        | None      -> []
        | Some x    -> row x :: loop (fetch r)
    in
        loop (fetch r)


