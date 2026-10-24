let annotate = Code_actions.code_action_test ~title:"Type-annotate"

let%expect_test "type-annotate selects the whole field access" =
  annotate
    {ocaml|
type state = { detached : int }
let f state = $state.detached$
|ocaml};
  [%expect
    {|
    type state = { detached : int }
    let f state = (state.detached : int)
    |}]
;;

let%expect_test "type-annotate selects the record when only its name is selected" =
  annotate
    {ocaml|
type state = { detached : int }
let f state = $state$.detached
|ocaml};
  [%expect
    {|
    type state = { detached : int }
    let f state = (state : state).detached
    |}]
;;

let%expect_test "type-annotate preserves cursor behavior before a field access" =
  annotate
    {ocaml|
type state = { detached : int }
let f state = $state.detached
|ocaml};
  [%expect
    {|
    type state = { detached : int }
    let f state = (state : state).detached
    |}]
;;

let%expect_test "type-annotate encloses a partial selection of a field access" =
  annotate
    {ocaml|
type state = { detached : int }
let f state = $state.det$ached
|ocaml};
  [%expect
    {|
    type state = { detached : int }
    let f state = (state.detached : int)
    |}]
;;

let%expect_test "type-annotate encloses a partial selection of a record name" =
  annotate
    {ocaml|
type state = { detached : int }
let f state = $sta$te.detached
|ocaml};
  [%expect
    {|
    type state = { detached : int }
    let f state = (state : state).detached
    |}]
;;

let%expect_test "type-annotate encloses a selection starting inside the record name" =
  annotate
    {ocaml|
type state = { detached : int }
let f state = sta$te.det$ached
|ocaml};
  [%expect
    {|
    type state = { detached : int }
    let f state = (state.detached : int)
    |}]
;;

let%expect_test "type-annotate selects a nested arithmetic expression" =
  annotate
    {ocaml|
let f x = ($x * 2$) + 3
|ocaml};
  [%expect {| let f x = ((x * 2) : int) + 3 |}]
;;

let%expect_test "type-annotate selects a function call" =
  annotate
    {ocaml|
let f x = $string_of_int x$
|ocaml};
  [%expect {| let f x = (string_of_int x : string) |}]
;;

let%expect_test "type-annotate selects a multiline expression" =
  annotate
    {ocaml|
let f x = $x +
  1$
|ocaml};
  [%expect
    {|
    let f x = (x +
      1 : int)
    |}]
;;

let%expect_test "type-annotate checks the selected parent instead of its annotated child" =
  annotate
    {ocaml|
let f x = $(x : int) + 1$
|ocaml};
  [%expect {| let f x = ((x : int) + 1 : int) |}]
;;

let%expect_test "type-annotate rejects a selected annotated expression" =
  annotate {ocaml|let f x = $(x : int)$ + 1|ocaml};
  [%expect {| |}]
;;

let%expect_test "type-annotate rejects a selected coerced expression" =
  annotate {ocaml|let f x = $(x : int :> int)$ + 1|ocaml};
  [%expect {| |}]
;;

let%expect_test "type-annotate rejects a selected annotated pattern" =
  annotate {ocaml|let f $(x : int)$ = x + 1|ocaml};
  [%expect {| |}]
;;

let%expect_test "type-annotate rejects a selection spanning structure items" =
  annotate
    {ocaml|
let x = $1
let y = 2$
|ocaml};
  [%expect {| |}]
;;
