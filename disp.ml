(*
 * disp.ml
 *
 * addressタイプの要素でできたリストを一覧表示する
 *)

let onedata = ref ""

let rec disp_address_list = function
    [] -> ""
    | m :: rest ->
            onedata := 
            (string_of_int m.id) ^ " " ^ m.firstname
            ^ " " ^ m.lastname
            ^ " " ^ m.sei
            ^ " " ^ m.namae
            ^ " " ^ m.email
            ^ " " ^ m.memo;
            print_endline !onedata;
            disp_address_list rest

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

