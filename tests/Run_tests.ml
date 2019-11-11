open OUnit2;;

let () = run_test_tt_main TestOctet.suite
let () = run_test_tt_main TestPawn.suite
let () = run_test_tt_main TestRook.suite
let () = run_test_tt_main TestKnight.suite
let () = run_test_tt_main TestBishop.suite
let () = run_test_tt_main TestQueen.suite
let () = run_test_tt_main TestKing.suite
let () = run_test_tt_main TestEndGame.suite
;;
