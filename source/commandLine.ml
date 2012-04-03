exception Unknown of string;;
exception ArgumentRequired of string;;
exception ArgumentNotRequired of string;;

type 'a cl_option = {
	short: string;
	long: string;
	desc: string;
	has_argument: bool;
	apply: string -> 'a -> 'a};;

let case
	(short: string)
	?(long: string = "")
	?(desc: string = "")
	(has_argument: bool)
	(apply: (string -> 'a -> 'a))
	: 'a cl_option =
(
	let long =
		if long = "" && String.length short > 1 then short else long
	in
	{short; long; desc; has_argument; apply}
);;

let otherwise
	(apply: (string -> 'a -> 'a))
	: 'a cl_option =
(
	{short = ""; long = ""; desc = ""; has_argument = true; apply}
);;

let ( => )
	(partial: bool -> (string -> 'a -> 'a) -> 'a cl_option)
	(apply: 'a -> 'a)
	: 'a cl_option =
(
	partial false (fun _ -> apply)
);;

let ( =>? )
	(partial: bool -> (string -> 'a -> 'a) -> 'a cl_option)
	(apply: string -> 'a -> 'a)
	: 'a cl_option =
(
	partial true apply
);;

let parse
	(spec: 'a cl_option list)
	(argv: string array)
	(options: 'a): 'a =
(
	let rec loop i options = (
		if i >= Array.length argv then options else
		let arg = argv.(i) in
		if arg.[0] = '-' then (
			let p, param =
				let rec loop arg i = (
					if i >= String.length arg then String.length arg, None else
					begin match arg.[i] with
					| '=' ->
						let n = i + 1 in
						let param = String.sub arg n (String.length arg - n) in
						i, Some param
					| '.' | '/' ->
						let n = i in
						let param = String.sub arg n (String.length arg - n) in
						i, Some param
					| _ ->
						loop arg (i + 1)
					end
				) in
				loop arg 1
			in
			let opt, lors =
				if arg.[1] = '-' then (
					String.sub arg 2 (p - 2), `long
				) else (
					String.sub arg 1 (p - 1), `short
				)
			in
			let rec apply_loop xs options = (
				begin match xs with
				| {short; long; has_argument; apply} :: _
					when (match lors with `short -> short = opt | `long -> long = opt)
				->
					if has_argument then (
						begin match param with
						| Some param ->
							loop (i + 1) (apply param options)
						| None ->
							let ip1 = i + 1 in
							if ip1 >= Array.length argv then (
								raise (ArgumentRequired arg)
							) else (
								loop (ip1 + 1) (apply argv.(ip1) options)
							)
						end
					) else (
						if param <> None then (
							raise (ArgumentNotRequired arg)
						);
						loop (i + 1) (apply "" options)
					)
				| _ :: xr ->
					apply_loop xr options
				| [] ->
					raise (Unknown arg)
				end
			) in
			apply_loop spec options
		) else (
			let rec apply_loop xs options = (
				begin match xs with
				| {short = ""; apply} :: _ ->
					loop (i + 1) (apply arg options)
				| _ :: xr ->
					apply_loop xr options
				| [] ->
					raise (Unknown arg)
				end
			) in
			apply_loop spec options
		)
	) in
	loop 1 options
);;

let output_spec
	(f: out_channel)
	(spec: 'a cl_option list)
	: unit =
(
	let option_width =
		List.fold_left (fun w x ->
			if x.desc = "" then w else
			let c =
				if x.short = "" || x.long = x.short then 0 else (
					1 + String.length x.short
				)
			in
			let c =
				if x.long = "" then c else (
					let c =
						if c = 0 then 0 else (
							c + 1
						)
					in
					c + 2 + String.length x.long
				)
			in
			let c =
				if not x.has_argument then c else (
					c + 4
				)
			in
			max w c
		) 0 spec
	in
	List.iter (fun x ->
		if x.desc <> "" && (x.short <> "" || x.long <> "") then (
			output_string f "  ";
			let c =
				if x.short = "" || x.long = x.short then 0 else (
					output_string f "-";
					output_string f x.short;
					1 + String.length x.short
				)
			in
			let c =
				if x.long = "" then c else (
					let c =
						if c = 0 then 0 else (
							output_string f " ";
							c + 1
						)
					in
					output_string f "--";
					output_string f x.long;
					c + 2 + String.length x.long
				)
			in
			let c =
				if not x.has_argument then c else (
					output_string f "=...";
					c + 4
				)
			in
			output_string f (String.make (option_width - c) ' ');
			output_string f " ";
			output_string f x.desc;
			output_string f "\n"
		)
	) spec
);;

let print_usage spec = output_spec stdout spec;;
