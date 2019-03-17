module Bob = struct 

	(* string -> string list *)
	let read_file filename = 
		let lines = ref [] in
		let chan = open_in filename in
		try
			while true; do
				lines := input_line chan :: !lines
			done; !lines
		with End_of_file ->
			close_in chan;
		List.rev !lines
	;;

	(* string -> char list *)
	let explode (s : string) =
		let rec exp i l =
			if i < 0 then l else exp (i - 1) (s.[i] :: l)
		in
		exp (String.length s - 1) []
	;;

	(* int -> 'a list -> 'a list
	returns first n elements *)
	let rec take n lst=
		match lst with 
		| [] -> []
		| hd :: tl -> if n = 1 then [hd] else hd::take (n - 1) tl
	;;

	(* int -> 'a list -> 'a list
	returns last n elements *)
	let rec cut n lst =
		match n with
		| 0 -> lst
		| _ -> cut (n - 1) (List.tl lst);
		
	;;

	(* int -> 'a list -> 'a list
	move_to_front 2 [1;2;3;4;5] = [3;1;2;4;5] *)
	let move_to_front n lst =
		let nth = List.nth lst n in
		let hd = List.hd lst in
		let l = take (n - 1) lst in
		let r = cut n lst in
		List.flatten [[nth]; l; [hd]; r]
	;;

	(* string list -> unit *)
	let print_string_list lst =
		List.iter (Printf.printf "%s") lst
	;;

	(* char list -> unit *)
	let print_char_list lst =
		List.iter (Printf.printf "%c") lst
	;;

	(* int -> 'a list -> 'a list
	duplicate_nth_list_item 1 [1;2;3] = [1;2;2;3] *)
	let duplicate_nth_list_item n lst =
		let dup = List.nth lst n in
		let l = take n lst in
		let r = cut n lst in
		List.append (List.append l [dup]) r
	;;

	(* char list -> string list *)
	let char_list_to_string lst =
		List.fold_left (fun base next -> base ^ (Char.escaped next)) "" lst
	;;

	(* char list -> string *)
	let rec char_list_to_str_list lst =
		[(char_list_to_string lst)]
	;;

	(* string list -> string *)
	let rec str_list_to_string lst =
		match lst with
		| [] -> ""
		| [x] -> x
		| hd :: tl -> hd ^ str_list_to_string tl
	;;

	(* int -> bool
	C standard *)
	let int_to_bool x = if x = 0 then false else true ;;

	(* string opt -> int
	throws Failure: int_of_string *)
	let string_opt_to_int o =
		match o with
	    | Some v -> int_of_string v
	    | None -> -1
	;;

	let string_opt_to_string o =
		match o with
	    | Some v -> v
	    | None -> ""
	;;

	(* int -> string *)
	let rand len = 
	    let urandom = open_in "/dev/urandom" in
	    let ret = Bytes.create len in
	    input urandom ret 0 len;
	    Bytes.to_string ret
	;;
		
	let int_of_char ch =
		int_of_string ("0x0" ^ (Char.escaped ch) )
	;;

	let http_post url2 body2 = 
		let open Cohttp_lwt_unix in
		let open Lwt in
		let open Cohttp in
		Client.post ~body:(Cohttp_lwt__Body.of_string body2) (Uri.of_string url2) >>= fun (resp, body) ->
			let code = resp |> Response.status |> Code.code_of_status in
			body |> Cohttp_lwt.Body.to_string >|= fun body ->
			Lwt_io.printf "Body: %s\n" body;
		body
	;;

end