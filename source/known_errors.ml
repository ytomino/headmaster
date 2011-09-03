open Position;;

module StringSet = Set.Make (String);;
module StringMap = Map.Make (String);;

let make_set (list: string list): StringSet.t = (
	List.fold_left (fun set filename ->
		StringSet.add filename set
	) StringSet.empty list
);;

let set_mem (ps: ranged_position) ~(set: StringSet.t): bool = (
	let filename, _, _, _ = fst ps in
	StringSet.mem (Filename.basename filename) set
);;

let make_setmap (list: (string * string list) list): StringSet.t StringMap.t = (
	List.fold_left (fun map (filename, macros) ->
		let set = List.fold_right StringSet.add macros StringSet.empty in
		StringMap.add filename set map
	) StringMap.empty list
);;

let setmap_mem (ps: ranged_position) (e: string) ~(set: StringSet.t StringMap.t): bool = (
	let filename, _, _, _ = fst ps in
	try
		let s = StringMap.find (Filename.basename filename) set in
		StringSet.mem e s
	with Not_found -> false
);;
