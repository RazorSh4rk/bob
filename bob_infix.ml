(*
Apply a function to a list, I use it for quick conversion.
[1;2;3] <-> string_of_int
*)
let (<->) _ls _f =
	List.map _f _ls
;;

(*
Same as settimeout in js, run a function with a delay.
(printf "") >||> 1.0
*)
let (>||>) _f _delay =
	let open Lwt in
	Lwt.bind (Lwt_unix.sleep _delay) (fun () -> Lwt.return _f)
;;

(*
Same as a List.contains would be
3 >? [1;2;3]
*)
let (>?) _n _ls =
	List.exists (fun el -> el = _n) _ls
;;