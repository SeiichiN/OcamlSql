(*
 * inputData.ml
 *)
(* #use "type.ml" *)

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

(* make_insert_sql (input_data ()) *)

