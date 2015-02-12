(* language *)

type language = [`c | `cxx | `objc | `objcxx];;

let cxx (lang: language): bool = (
	begin match lang with
	| `c | `objc -> false
	| `cxx | `objcxx -> true
	end
);;

let objc (lang: language): bool = (
	begin match lang with
	| `c | `cxx -> false
	| `objc | `objcxx -> true
	end
);;

module type LanguageType = sig
	val lang: language;;
end;;
