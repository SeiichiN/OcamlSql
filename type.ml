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
