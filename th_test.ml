(* p240 *) 

let work n () =
     print_int n;
     flush stdout;
     Thread.delay 1.0;;

let rec loop f n arg =
    if n <= 0 then ()
    else loop f (n - 1) (f arg);;

let _ = 
    Thread.create (loop (work 1) 5) () (* 5回ループ *)
in
let t2 =
    Thread.create (loop (work 2) 5) ()
in
Thread.join t2;;

