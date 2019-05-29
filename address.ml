(**
   アドレス帳
   
   $Id: address.ml,v 0.1 2019/05/21 Seiichi Nukayama $
   Copyright (c) 2019 Seiichi Nukayama, All Rights Reserved.

   HowTo compilation
   $ ocamlfind ocamlopt -o address -linkpkg -package mysql,str address.ml
*)

open Str
open Mysql

(*******************************************
 * type
 *******************************************)
type address = {
    id : int;
    firstname : string;
    lastname  : string;
    sei       : string;
    namae     : string;
    email     : string;
    memo      : string
}

(*
 * @param:  [(        ); (         ); .... ]
 *
 * @return:  [{ address }; { address }; ...]
 *)
let rec mkRecord = function
    [] -> []
    | (id_n, f_n, l_n, sei_n, namae_n, e_n, m_n) :: rest ->
            let mo =
                {id = id_n;
                firstname = f_n;
                lastname = l_n;
                sei = sei_n;
                namae = namae_n;
                email = e_n;
                memo = m_n } in
            mo :: (mkRecord rest)

let get_field n r =
    match n with
    0 -> ""
    | 1 -> string_of_int r.id
    | 2 -> r.firstname
    | 3 -> r.lastname
    | 4 -> r.sei
    | 5 -> r.namae
    | 6 -> r.email
    | 7 -> r.memo
    | _ -> "" 

let field_list = [
    (1, "id"); (2, "firstname"); (3, "lastname"); (4, "sei"); (5, "namae");
    (6, "email"); (7, "memo")
    ]

let select_list = [
    (1, "id番号 > "); (2, "FirstName > "); (3, "LastName > "); (4, "姓 > ");
    (5, "名前 > "); (6, "メールアドレス > ") ]


(*************************************************
readfile.ml
ファイルから設定情報を読み込む
 *************************************************)

(* リストのリストを 連想リストに変換する *)
let rec mk_assoc_list = function
    [] -> []
    | (_::_::_::_)::_ -> []
    | (_::[])::_ -> []
    | []::_ -> []
    | [a; b] :: rest ->
            (a, b) :: mk_assoc_list rest


let rec input_conf ch =
  (* ファイルの終了は、-- をわたす *)
  let s = try input_line ch with End_of_file -> "--" in
  (* 先頭が ; の行はコメントなので、パス *)
  if string_match (regexp "^;.") s 0 then input_conf ch
  else
      (* 先頭が -- の行は、終了 *)
      if string_match (regexp "^--") s 0
      then []
      else
          (* 空白を区切りとしてリストにする *)
          let conf =
              split (regexp "[ \t]+") s in
          conf :: input_conf ch

let read_conf filename =
  let ch = open_in filename in
  let l = input_conf ch in
  close_in ch;
  mk_assoc_list l


(************************************
 * listAll
 ************************************)


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



(***************************************************
 * disp.ml
 *
 * addressタイプの要素でできたリストを一覧表示する
 ***************************************************)

let onedata = ref ""

let rec print_ichiran = function
    [] -> ""
    | m :: rest ->
      onedata := Printf.sprintf 
          "%-3s %-12s %-12s %-4s %-4s %-20s %-50s" 
            (string_of_int m.id)
            m.firstname
            m.lastname
            m.sei
            m.namae
            m.email
            m.memo;
      print_endline !onedata;
            print_ichiran rest

let disp_address_list l =
    print_endline "------------------------ 一覧 -------------------------";
    ignore (print_ichiran l);
    print_endline "-------------------------------------------------------"


(*
 * addressタイプの要素でできた（１つの）リストを表示する
 * @param: list of type address
 *
 * もしも、検索の結果、複数のデータがあれば、複数データを
 * 表示する。
 *)
let print_one m = 
    print_endline "------------- データ ----------------";
    Printf.printf "    id:        %d\n" m.id;
    Printf.printf " 1) FirstName: %s\n" m.firstname;
    Printf.printf " 2) LastName:  %s\n" m.lastname;
    Printf.printf " 3) 姓:        %s\n" m.sei;
    Printf.printf " 4) 名前:      %s\n" m.namae;
    Printf.printf " 5) メルアド:  %s\n" m.email;
    Printf.printf " 6) メモ:      %s\n" m.memo;
    print_endline "-------------------------------------"

let rec disp_select_list = function
    [] -> ""
    | m :: rest ->
            print_one m
            ; disp_select_list rest



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
       

(************************************************************
 * syori.ml
 ************************************************************)

let pr_sentaku () =
  print_newline ();
  print_endline "---------- データの閲覧・検索・訂正 ----------";
  print_endline "|                                            |";
  print_endline "| 何をキーとしてデータを検索しますか？       |";
  print_endline "|                                            |";
  print_endline "|   1) id番号                                |";
  print_endline "|   2) FirstName                             |";
  print_endline "|   3) LastName                              |";
  print_endline "|   4) 姓                                    |";
  print_endline "|   5) 名前                                  |";
  print_endline "|   6) メールアドレス                        |";
  print_endline "|   0) 中止                                  |";
  print_endline "|                                            |";
  print_endline "----------------------------------------------";
  print_string  " 番号? > ";
  flush stdout;
  let bango = (input_line stdin) in bango

let pr_menu () =
    print_newline ();
    print_endline "------------- アドレス帳 ---------------";
    print_endline "|  1) データの追加                     |";
    print_endline "|  2) データの検索・訂正               |";
    print_endline "|  3) データの一覧                     |";
    print_endline "|  4) 削除処理                         |";
    print_endline "|  0) 終了                             |";
    print_endline "| -----------------------------------  |";
    print_endline "| Copyright (c) 2019 Seiichi Nukayama  |";
    print_endline "| http://www.billies-works.com/        |";
    print_endline "----------------------------------------";
    print_newline ();
    print_string "番号を入力してください > ";
    flush stdout;
    let bango = input_line stdin in
    bango

let pr_edit () =
    print_string "1) 訂正    0) もどる > ";
    flush stdout;
    let bango = input_line stdin in
    bango

let pr_edit_select () =
    print_endline "\nどの項目を訂正しますか？ 番号で指定してください。";
    print_string "番号? (0:中止) > ";
    flush stdout;
    let bango = input_line stdin in
    bango

let pr_reselect n =
  print_endline ("データが " ^ (string_of_int n) ^ " 個あります。");
  print_endline ("id番号でデータを選択してください。");
  print_string "id番号 > ";
  flush stdout;
  let bango = input_line stdin in
  bango

let pr_delete_kakunin () =
    print_string "本当に削除してよろしいですか？ (y/n) > ";
    flush stdout;
    let yesno = String.lowercase_ascii (input_line stdin) in
    if yesno = "y"
    then true  (* 削除OK *)
    else false  (* NG *)

let pr_delete_id () =
    print_string "削除したいデータの id > ";
    flush stdout;
    let bango = input_line stdin in
    bango

(*
 * syori -- 画面にメニューを表示し、ユーザーに数字キーを入力させて
 *          処理を選択させる
 * @param: int max -- メニューの最大番号
 *         func f  -- 画面にメニューを表示し、ユーザーから数字を得る関数
 *                    この関数は数字を文字列で返すようにする
 * @return: !num -- 数字
 *)
let syori max f =
    let num = ref (max + 1) in
    while ( !num > max || !num < 0 ) do
        try
            num := int_of_string (f ())  (* 文字列を数値に変換 *)
            ; if ( !num > max || !num < 0 )
            then print_endline ("0 〜 " ^ (string_of_int max) ^ " の数字です。")
            else ()
        with Failure a ->
            print_endline "番号ではありませんでした"
            ; num := (max + 1)
    done
    ; !num

let etsuran () = syori 6 pr_sentaku    

let menu () = syori 4 pr_menu

(* 修正したい項目を番号で返す。 0 の場合は中止 *)
let edit () = 
    let num = syori 1 pr_edit in
    if num = 1 
    then let item_no = syori 6 pr_edit_select in item_no
    else 0

(* f -- 条件を記述した関数 *)
let rec myfind f = function
    [] -> []
  | a :: rest ->
     if ((f a) = true) then [a] else myfind f rest
                             
           
(*
  @param: l -- レコード・リスト（address型）

  @return: 一つだけのリスト
 *)
let reselect l =
  let n = List.length l in
  if n > 1
  then
    let id_num = int_of_string (pr_reselect n) in
    let newList = myfind (fun x -> x.id = id_num) l in
    newList
  else
    l

let r1 = {id=1; firstname="F"; lastname="L"; sei="あ"; namae="い"; email="e"; memo="m"}
let r2 = {id=2; firstname="G"; lastname="H"; sei="か"; namae="き"; email="l"; memo="n"}


(*
 * どれかのフィールドを番号で指定し、新しい値を入力する関数
 *
 * @param: n -- 1) firstname,  2) lastname, 3) sei, 4) namae
 *              5) email       6) memo
 *         l -- addressList [{id, firstname,...., memo}{ .... }] -- address list
 *
 * @return: newValue
 *)
let retouch_data n l =
    let oneR = List.hd l in
    let fldvalue = get_field (n + 1) oneR in
    let fldmessage = assoc (n+1) select_list in
    let fldname = assoc (n+1) field_list in
    let mes_str = "現在の設定： " ^ fldmessage ^ " = " ^ fldvalue in
    print_endline mes_str;
    print_string "新しい値 > ";
    flush stdout;
    let newValue = input_line stdin in
    (oneR.id, fldname, newValue)

(*
 * 削除処理
 * @return: delete_id -- 削除するid番号（数値）
 *          0         -- 削除しない
 *)
let select_delete_id () =
    let delete_id = syori 99 pr_delete_id in
    if (pr_delete_kakunin())
    then delete_id
    else 0


(*******************************************************
 * inputData.ml
 ******************************************************)

let input_data () =
    let f_name = ref ""
    and l_name = ref ""
    and sei_name = ref ""
    and namae_name = ref ""
    and email_n = ref ""
    and memo_n = ref "" in
    print_endline "--------- データの追加 ------------";
    print_string "first name > ";
    flush stdout;
    f_name := input_line stdin;
    print_string "last name > ";
    flush stdout;
    l_name := input_line stdin;
    print_string "sei > ";
    flush stdout;
    sei_name := input_line stdin;
    print_string "namae > ";
    flush stdout;
    namae_name := input_line stdin;
    print_string "email > ";
    flush stdout;
    email_n := input_line stdin;
    print_string "memo > ";
    flush stdout;
    memo_n := input_line stdin;
    let person = {
        id = 0;
        firstname = !f_name;
        lastname = !l_name;
        sei = !sei_name;
        namae = !namae_name;
        email = !email_n;
        memo = !memo_n
    } in
    person

let make_insert_sql m =
    let sql = "insert into " ^ tablename ^ " ( lastname, firstname, sei, namae, email, memo ) 
    values (" ^ (ml2str m.lastname) ^ "," ^ (ml2str m.firstname) ^ "," 
    ^ (ml2str m.sei) ^ "," ^ (ml2str m.namae) ^ "," ^ (ml2str m.email)
    ^ "," ^ (ml2str m.memo) ^ ")" in
    sql;;


(********************************************************
 *  main
 ********************************************************)


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
