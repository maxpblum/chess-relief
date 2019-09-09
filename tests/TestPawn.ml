open OUnit2;;

(* TODO: Replace with actual pawn test *)
let test_to_list_and_set test_ctxt =
    assert_equal
        ~msg:"octet converted to list should have expected content"
        Octet.(init 1 |> set 0 0 |> set 2 5 |> to_list)
        [0; 1; 5; 1; 1; 1; 1; 1]

(* Name the test cases and group them together *)
let suite =
"suite">:::
 [
     "test to_list and set">:: test_to_list_and_set;
 ]
;;
