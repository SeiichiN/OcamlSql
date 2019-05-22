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

