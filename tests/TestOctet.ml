open OUnit2;;

let string_of_int_option = function
| None -> "None"
| Some x -> String.concat "" ["Some "; string_of_int x]

let string_of_int_list l =
    String.concat "" ["["; (List.map string_of_int l |> String.concat "; "); "]"]

let test_octet_init_and_get test_ctxt =
	assert_equal
		~msg:"all 1s"
		~printer:string_of_int_option
		(Some 1)
		Octet.(init 1 |> get 3)

let test_out_of_bounds test_ctxt =
	let () = assert_equal
		~msg:"index too low"
		~printer:string_of_int_option
		None
		Octet.(init 1 |> get (-1)) in
	assert_equal
		~msg:"index too high"
		~printer:string_of_int_option
		None
		Octet.(init 1 |> get 8)

let test_equality test_ctxt =
	let make_octet () = Octet.(init 1 |> set 0 0 |> set 2 5) in
	assert_equal
		~msg:"separately constructed octets should be equal"
        (make_octet ()) (make_octet ())

let test_to_list_and_set test_ctxt =
    assert_equal
        ~msg:"octet converted to list should have expected content"
        ~printer:string_of_int_list
        Octet.(init 1 |> set 0 0 |> set 2 5 |> to_list)
        [0; 1; 5; 1; 1; 1; 1; 1]

(* Name the test cases and group them together *)
let suite =
 ["test octet init and get">:: test_octet_init_and_get;
  "test out of bounds">:: test_out_of_bounds;
  "test equality">:: test_equality;
  "test to_list and set">:: test_to_list_and_set;
 ]
;;
