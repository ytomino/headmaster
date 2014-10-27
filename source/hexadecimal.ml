let uppercase = "0123456789ABCDEF";;
let lowercase = "0123456789abcdef";;

let x4 (case: string) (x: Int32.t): string = (
	let s = Bytes.make 4 '_' in
	Bytes.set s 0 case.[Int32.to_int (Int32.shift_right x 12) land 15];
	Bytes.set s 1 case.[Int32.to_int (Int32.shift_right x 8) land 15];
	Bytes.set s 2 case.[Int32.to_int (Int32.shift_right x 4) land 15];
	Bytes.set s 3 case.[Int32.to_int x land 15];
	Bytes.unsafe_to_string s
);;

let x8 (case: string) (x: Int32.t): string = (
	let s = Bytes.make 8 '_' in
	Bytes.set s 0 case.[Int32.to_int (Int32.shift_right x 28) land 15];
	Bytes.set s 1 case.[Int32.to_int (Int32.shift_right x 24) land 15];
	Bytes.set s 2 case.[Int32.to_int (Int32.shift_right x 20) land 15];
	Bytes.set s 3 case.[Int32.to_int (Int32.shift_right x 16) land 15];
	Bytes.set s 4 case.[Int32.to_int (Int32.shift_right x 12) land 15];
	Bytes.set s 5 case.[Int32.to_int (Int32.shift_right x 8) land 15];
	Bytes.set s 6 case.[Int32.to_int (Int32.shift_right x 4) land 15];
	Bytes.set s 7 case.[Int32.to_int x land 15];
	Bytes.unsafe_to_string s
);;

let x4u = x4 uppercase;;
let x4l = x4 lowercase;;

let x8u = x8 uppercase;;
let x8l = x8 lowercase;;
