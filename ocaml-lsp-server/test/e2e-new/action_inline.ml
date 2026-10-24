let inline_test = Code_actions.code_action_test ~title:"Inline into uses"

let%expect_test "" =
  inline_test
    {|
let _ =
  let $x = 0 in
  x + 1
|};
  [%expect
    {|
    let _ =
      let x = 0 in
      (0) + 1 |}]
;;

let%expect_test "shadow-1" =
  inline_test
    {|
let _ =
  let y = 1 in
  let $x = y in
  let y = 0 in
  x + 1
|};
  [%expect {| |}]
;;

let%expect_test "shadow-2" =
  inline_test
    {|
let _ =
  let y = 1 in
  let $x y = y in
  let y = 0 in
  x y + 1
|};
  [%expect
    {|
    let _ =
      let y = 1 in
      let x y = y in
      let y = 0 in
      (fun y -> y) y + 1 |}]
;;

let%expect_test "shadow-3" =
  inline_test
    {|
let _ =
  let y = 1 in
  let $x z = y + z in
  let y = 0 in
  x y + 1
|};
  [%expect {| |}]
;;

let%expect_test "shadow-4" =
  inline_test
    {|
module M = struct
  let y = 1
end
let _ =
  let $x = M.y in
  let module M = struct
    let y = 2
  end in
  x
|};
  [%expect {| |}]
;;

let%expect_test "shadow-5" =
  inline_test
    {|
module M = struct
  let y = 1
end
let _ =
  let $x = M.y in
  let module N = struct
    let y = 2
  end in
  x
|};
  [%expect
    {|
    module M = struct
      let y = 1
    end
    let _ =
      let x = M.y in
      let module N = struct
        let y = 2
      end in
      (M.y) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $x = 0 + 1 in
  (fun x -> x) x
|};
  [%expect {| |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $x = 0 + 1 in
  (fun ~x -> x) ~x
|};
  [%expect {| |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $x = 0 + 1 in
  (fun ?(x = 2) -> x) ~x
|};
  [%expect {| |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $x = Some 0 in
  (fun ?(x = 2) -> x) ?x
|};
  [%expect {| |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $x = 0 in
  (fun ~x -> x) ~x:(x + 1)
|};
  [%expect
    {|
    let _ =
      let x = 0 in
      (fun ~x -> x) ~x:((0) + 1) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $x = 0 in
  (fun ?(x = 1) -> x) ~x:(x + 1)
|};
  [%expect
    {|
    let _ =
      let x = 0 in
      (fun ?(x = 1) -> x) ~x:((0) + 1) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f x = x in
  f 1
|};
  [%expect
    {|
    let _ =
      let f x = x in
      (fun x -> x) 1 |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f _ = 0 in
  f 1
|};
  [%expect
    {|
    let _ =
      let f _ = 0 in
      (fun _ -> 0) 1 |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f x = x + x in
  f 1
|};
  [%expect
    {|
    let _ =
      let f x = x + x in
      (fun x -> x + x) 1 |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f x = x + x in
  f (g 1)
|};
  [%expect
    {|
    let _ =
      let f x = x + x in
      (fun x -> x + x) (g 1) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f x y = x + y in
  f 0
|};
  [%expect {| |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f x ~y = x + y in
  f ~y:0
|};
  [%expect {| |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f ~x y = x + y in
  f ~x:0
|};
  [%expect {| |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f ~x ~y = x + y in
  f ~y:0
|};
  [%expect {| |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f (x : int) = x + 1 in
  f 0
|};
  [%expect
    {|
    let _ =
      let f (x : int) = x + 1 in
      (fun (x : int) -> x + 1) 0 |}]
;;

(* TODO: allow beta reduction with locally abstract types *)
let%expect_test "" =
  inline_test
    {|
let _ =
  let $f (type a) (x : a) = x in
  f 0
|};
  [%expect {| |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f : int -> int = fun x -> x in
  f 0
|};
  [%expect
    {|
    let _ =
      let f : int -> int = fun x -> x in
      (fun x -> x) 0 |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f = function Some x -> x | None -> 0 in
  f (Some 1)
|};
  [%expect
    {|
    let _ =
      let f = function Some x -> x | None -> 0 in
      (function | Some x -> x | None -> 0) (Some 1) |}]
;;

(* TODO: allow beta reduction with `as` *)
let%expect_test "" =
  inline_test
    {|
let _ =
  let $f (x as y) = y + 1 in
  f 1
|};
  [%expect
    {|
    let _ =
      let f (x as y) = y + 1 in
      (fun (x as y) -> y + 1) 1 |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f 1 = 2 in
  f 2
|};
  [%expect
    {|
    let _ =
      let f 1 = 2 in
      (fun 1 -> 2) 2 |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f (x, y) = x + y in
  f (1, 2)
|};
  [%expect
    {|
    let _ =
      let f (x, y) = x + y in
      (fun (x, y) -> x + y) (1, 2) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f (x, y) = x + y + y in
  f (1, 2 + 3)
|};
  [%expect
    {|
    let _ =
      let f (x, y) = x + y + y in
      (fun (x, y) -> (x + y) + y) (1, 2 + 3) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f (x, y) = x + y + y in
  let z = (1, 2) in
  f z
|};
  [%expect
    {|
    let _ =
      let f (x, y) = x + y + y in
      let z = (1, 2) in
      (fun (x, y) -> (x + y) + y) z |}]
;;

(* TODO *)
let%expect_test "" =
  inline_test
    {|
type t = { x : int; y : int }
let _ =
  let $f { x; y } = x + y in
  f { x = 1; y = 1 }
|};
  [%expect
    {|
    type t = { x : int; y : int }
    let _ =
      let f { x; y } = x + y in
      (fun { x; y } -> x + y) { x = 1; y = 1 } |}]
;;

(* TODO: beta reduce record literals as with tuples *)
let%expect_test "" =
  inline_test
    {|
type t = { x : int; y : int }
let _ =
  let $f { x; _ } = x + 1 in
  f { x = 1; y = 1 }
|};
  [%expect
    {|
    type t = { x : int; y : int }
    let _ =
      let f { x; _ } = x + 1 in
      (fun { x;_} -> x + 1) { x = 1; y = 1 } |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f x = [%test] x in
  f 1
|};
  [%expect
    {|
    let _ =
      let f x = [%test] x in
      (fun x -> ([%test ]) x) 1 |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f x = x in
  [%test] (f 1)
|};
  [%expect
    {|
    let _ =
      let f x = x in
      [%test] ((fun x -> x) 1) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f x = (* test comment *) x in
  f 1
|};
  [%expect {| |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f x = x in
  (* test comment *) f 1
|};
  [%expect
    {|
    let _ =
      let f x = x in
      (* test comment *) (fun x -> x) 1 |}]
;;

let%expect_test "" =
  inline_test
    {|
let $f x = x
let g y = f y
|};
  [%expect
    {|
    let f x = x
    let g y = (fun x -> x) y |}]
;;

(* TODO *)
let%expect_test "" =
  inline_test
    {|
module M = struct
  let $f x = x
  let g y = f y
end
let h = M.f
|};
  [%expect
    {|
    module M = struct
      let f x = x
      let g y = (fun x -> x) y
    end
    let h = M.f |}]
;;
