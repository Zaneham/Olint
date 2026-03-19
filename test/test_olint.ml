(* test_olint.ml — the prosecution calls its witnesses.
   Each fixture is parsed and checked against expected
   rule IDs and line numbers. No test framework dependency
   because we have opinions about that sort of thing. *)

let pass = ref 0
let fail = ref 0

let chk name cond =
  if cond then (
    incr pass;
    Printf.printf "  PASS: %s\n" name
  ) else (
    incr fail;
    Printf.printf "  FAIL: %s\n" name
  )

let has rid ds =
  List.exists (fun (d : Olint_lib.Diagnostic.t) -> d.rid = rid) ds

let cnt rid ds =
  List.length (List.filter (fun (d : Olint_lib.Diagnostic.t) -> d.rid = rid) ds)

let fpath f = "test/fixtures/" ^ f

(* slurp helper for comparing file contents *)
let slurp path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

(* write helper for test setup *)
let spit path s =
  let oc = open_out_bin path in
  output_string oc s;
  close_out oc

let () =
  Printf.printf "olint test suite\n";
  Printf.printf "================\n\n";

  (* W001: unused open *)
  Printf.printf "[W001] unused open\n";
  let ds = Olint_lib.Olint.lint (fpath "unused_open.ml") in
  chk "fires on unused Printf" (has "W001" ds);
  chk "exactly one W001" (cnt "W001" ds = 1);

  (* W002: redundant match *)
  Printf.printf "\n[W002] redundant match\n";
  let ds = Olint_lib.Olint.lint (fpath "redundant_match.ml") in
  chk "fires on identity match" (has "W002" ds);
  chk "catches both matches" (cnt "W002" ds = 2);

  (* W003/W004: list antipatterns *)
  Printf.printf "\n[W003] list length compare\n";
  let ds = Olint_lib.Olint.lint (fpath "list_antipatterns.ml") in
  chk "fires on List.length = 0" (has "W003" ds);
  chk "catches both length checks" (cnt "W003" ds = 2);

  Printf.printf "\n[W004] list nth\n";
  chk "fires on List.nth" (has "W004" ds);

  (* W005: partial functions *)
  Printf.printf "\n[W005] partial functions\n";
  let ds = Olint_lib.Olint.lint (fpath "partial_functions.ml") in
  chk "fires on List.hd" (has "W005" ds);
  chk "catches all three" (cnt "W005" ds = 3);

  (* W006: naming *)
  Printf.printf "\n[W006] naming conventions\n";
  let ds = Olint_lib.Olint.lint (fpath "naming.ml") in
  chk "fires on camelCase" (has "W006" ds);
  chk "catches value and type" (cnt "W006" ds = 2);

  (* W008/W009/W010 + suppression *)
  Printf.printf "\n[W008] bool redundancy\n";
  let ds = Olint_lib.Olint.lint (fpath "new_rules.ml") in
  chk "fires on if-then-true-else-false" (has "W008" ds);
  chk "suppressed W008 not counted" (cnt "W008" ds = 2);

  Printf.printf "\n[W009] eta reduction\n";
  chk "fires on fun x -> succ x" (has "W009" ds);
  chk "suppressed W009 not counted" (cnt "W009" ds = 1);

  Printf.printf "\n[W010] exception control flow\n";
  chk "fires on Hashtbl.find in try" (has "W010" ds);

  (* clean file: no diagnostics *)
  Printf.printf "\n[clean] no false positives\n";
  let ds = Olint_lib.Olint.lint (fpath "clean.ml") in
  chk "zero diagnostics on clean file" (ds = []);

  (* autofix: fix fields populated *)
  Printf.printf "\n[autofix] fix fields\n";
  let ds = Olint_lib.Olint.lint (fpath "autofix.ml") in
  let fixable = List.filter (fun (d : Olint_lib.Diagnostic.t) -> d.fix <> []) ds in
  chk "W008 fixes populated" (List.exists (fun (d : Olint_lib.Diagnostic.t) ->
    d.rid = "W008" && d.fix <> []) ds);
  chk "W009 fix populated" (List.exists (fun (d : Olint_lib.Diagnostic.t) ->
    d.rid = "W009" && d.fix <> []) ds);
  chk "three fixable diagnostics" (List.length fixable = 3);

  (* autofix: apply produces expected output *)
  Printf.printf "\n[autofix] apply\n";
  let src = slurp (fpath "autofix.ml") in
  let tmp = fpath "autofix_tmp.ml" in
  spit tmp src;
  let n = Olint_lib.Olint.apply_file tmp in
  chk "three fixes applied" (n = 3);
  let got = slurp tmp in
  let exp = slurp (fpath "autofix_expected.ml") in
  chk "output matches expected" (got = exp);

  (* autofix: idempotent — second pass applies nothing *)
  let n2 = Olint_lib.Olint.apply_file tmp in
  chk "idempotent (0 on re-run)" (n2 = 0);
  Sys.remove tmp;

  (* summary *)
  Printf.printf "\n================\n";
  Printf.printf "%d passed, %d failed\n" !pass !fail;
  if !fail > 0 then exit 1 else exit 0
