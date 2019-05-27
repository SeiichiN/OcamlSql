(*
 * syori.ml
 *)

let pr_sentaku () =
    print_newline ()
    ; print_endline "---------- データの閲覧・検索・訂正 ----------"
    ; print_endline "|                                            |"
    ; print_endline "| 何をキーとしてデータを検索しますか？       |"
    ; print_endline "|                                            |"
    ; print_endline "|   1) id番号                                |"
    ; print_endline "|   2) FirstName                             |"
    ; print_endline "|   3) LastName                              |"
    ; print_endline "|   4) 姓                                    |"
    ; print_endline "|   5) 名前                                  |"
    ; print_endline "|   6) メールアドレス                        |"
    ; print_endline "|   0) 中止                                  |"
    ; print_endline "|                                            |"
    ; print_endline "----------------------------------------------"
    ; print_string  " 番号? > "
    ; flush stdout
    ; let bango = input_line stdin in
    bango

let pr_menu () =
    print_newline ();
    print_endline "------------- アドレス帳 ---------------";
    print_endline "|  1) データの追加                     |";
    print_endline "|  2) データの閲覧・検索・訂正         |";
    print_endline "|  3) データの削除                     |";
    print_endline "|  4) データの一覧                     |";
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
    print_string "1) 訂正   0) もどる > ";
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

(*
 * syori -- 画面にメニューを表示し、ユーザーに数字キーを入力させて
 *          処理を選択させる
 * @param: int max -- メニューの最大番号
 *         func f  -- 画面にメニューを表示し、ユーザーから数字を得る関数
 *)
let syori max f =
    let num = ref (max + 1) in
    while ( !num > max || !num < 0 ) do
        try
            num := int_of_string (f ())
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
    let fldname = get_field (n + 1) oneR in
    let mes_str = "現在の設定： " ^ (assoc (n+1) select_list) ^ " = " ^ fldname in
    print_endline mes_str;
    print_string "新しい値 > ";
    flush stdout;
    let newValue = input_line stdin in
    (oneR.id, fldname, newValue)
