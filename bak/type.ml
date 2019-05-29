(*
 * type.ml
 *)

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

