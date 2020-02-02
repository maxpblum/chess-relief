open OUnit2;;

let suite = "suite">::: (List.concat [
  TestOctet.suite;
  TestPawn.suite;
  TestRook.suite;
  TestKnight.suite;
  TestBishop.suite;
  TestQueen.suite;
  TestKing.suite;
  TestEndGame.suite;
  TestBasicPlay.suite;
])

let () = run_test_tt_main suite
;;
