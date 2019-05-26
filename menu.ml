(*
 * menu.ml
 *)

let pr_menu () =
    print_newline ();
    print_endline "---------- メニュー ---------------";
    print_endline " 1) データの追加";
    print_endline " 2) データの閲覧・検索・訂正";
    print_endline " 3) データの削除";
    print_endline " 4) データの一覧";
    print_endline " 0) 終了";
    print_endline "-----------------------------------";
    print_endline "Copyright (c) 2019 Seiichi Nukayama";
    print_endline "http://www.billies-works.com/";
    print_newline ();
    print_string "番号を入力してください > ";
    flush stdout;
    let bango = input_line stdin in
    bango

let menu () =
    let s = ref false 
    and num = ref 999 in
    while (!s = false) do
        try
            num := int_of_string (pr_menu());
        if (!num >= 0 && !num < 5)
        then s := true
        else print_endline "0 〜 4 の数字です。"
        with Failure a -> 
            print_endline "番号ではありませんでした。"
            ; exit 2
    done;
    !num

