open Test.Import

let unavailable title marked =
  let source, range = Code_actions.parse_selection marked in
  assert (Option.is_none (Code_actions.apply_code_action title source range));
  print_endline "unavailable"
;;

let preserves_runtime title marked =
  let source, range = Code_actions.parse_selection marked in
  let result = Code_actions.apply_code_action title source range |> Option.value_exn in
  let file = Filename.temp_file "ocamllsp-refactor-" ".ml" in
  let stem = Stdlib.Filename.chop_extension file in
  let binary = stem ^ ".exe" in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        [ file; stem ^ ".cmi"; stem ^ ".cmo"; binary ]
        ~f:(fun path -> if Sys.file_exists path then Sys.remove path))
    (fun () ->
       List.iter [ source; result ] ~f:(fun text ->
         let channel = open_out_bin file in
         output_string channel text;
         close_out channel;
         assert (
           Sys.command
             (Printf.sprintf
                "ocamlc -w -a -o %s %s"
                (Filename.quote binary)
                (Filename.quote file))
           = 0);
         assert (Sys.command (Filename.quote binary) = 0)));
  print_endline "preserved"
;;

let%expect_test "local extraction preserves conditional evaluation" =
  preserves_runtime
    "Extract local"
    {|
let calls = ref 0
let result = if false then $incr calls$ else ()
let () = assert (!calls = 0)
|};
  [%expect {| preserved |}]
;;

let%expect_test "local extraction preserves short circuit evaluation" =
  preserves_runtime
    "Extract local"
    {|
let calls = ref 0
let result = false && $(incr calls; true)$
let () = assert (not result && !calls = 0)
|};
  [%expect {| preserved |}]
;;

let%expect_test "local extraction stays inside a loop" =
  preserves_runtime
    "Extract local"
    {|
let calls = ref 0
let () = for i = 1 to 5 do $incr calls$ done
let () = assert (!calls = 5)
|};
  [%expect {| preserved |}]
;;

let%expect_test "local extraction preserves lazy evaluation" =
  preserves_runtime
    "Extract local"
    {|
let calls = ref 0
let value = lazy ($incr calls$)
let () = assert (!calls = 0); Lazy.force value; Lazy.force value; assert (!calls = 1)
|};
  [%expect {| preserved |}]
;;

let%expect_test "local extraction stays inside an exception handler" =
  preserves_runtime
    "Extract local"
    {|
let calls = ref 0
let () = try $failwith "expected"$ with Failure _ -> incr calls
let () = assert (!calls = 1)
|};
  [%expect {| preserved |}]
;;

let%expect_test "local extraction avoids name capture" =
  preserves_runtime
    "Extract local"
    {|
let var_name = 10
let result = var_name + $1$
let () = assert (result = 11)
|};
  [%expect {| preserved |}]
;;

let%expect_test "local extraction refuses to cross an effectful sibling" =
  unavailable
    "Extract local"
    {|
let calls = ref 0
let result = $1$ + (incr calls; 2)
|};
  [%expect {| unavailable |}]
;;

let%expect_test "function extraction avoids shadowing an existing function" =
  preserves_runtime
    "Extract function"
    {|
let fun_name x = x + 100
let f x = $(x * 2)$ + fun_name x
let () = assert (f 3 = 109)
|};
  [%expect {| preserved |}]
;;

let%expect_test "function extraction handles repeated free variables" =
  preserves_runtime
    "Extract function"
    {|
let f x = $x + x + x$
let () = assert (f 3 = 9)
|};
  [%expect {| preserved |}]
;;

let%expect_test "function extraction refuses an unavailable exception" =
  unavailable
    "Extract function"
    {|
let f () = let exception Local in $raise Local$
|};
  [%expect {| unavailable |}]
;;

let%expect_test "function extraction refuses ambiguous record labels" =
  unavailable
    "Extract function"
    {|
type a = { value : int }
type b = { value : string }
let f (x : a) = $x.value$
|};
  [%expect {| unavailable |}]
;;

let%expect_test "function extraction refuses an unavailable local type annotation" =
  unavailable
    "Extract function"
    {|
let f (type a) (x : a) = $let y : a = x in y$
|};
  [%expect {| unavailable |}]
;;

let%expect_test "inline refuses to duplicate a shared reference" =
  unavailable
    "Inline into uses"
    {|
let f () = let $x = ref 0 in incr x; !x
|};
  [%expect {| unavailable |}]
;;

let%expect_test "inline refuses an effectful initializer" =
  unavailable
    "Inline into uses"
    {|
let calls = ref 0
let $x = (incr calls; 1)
let result = x + x
|};
  [%expect {| unavailable |}]
;;

let%expect_test "inline preserves ignored argument effects" =
  preserves_runtime
    "Inline into uses"
    {|
let calls = ref 0
let $f _ = 1
let () = assert (f (incr calls) = 1); assert (!calls = 1)
|};
  [%expect {| preserved |}]
;;

let%expect_test "inline preserves argument order and single evaluation" =
  preserves_runtime
    "Inline into uses"
    {|
let log = ref []
let arg n = log := n :: !log; n
let $f x y = x + x + y
let () = assert (f (arg 1) (arg 2) = 4); assert (!log = [1; 2])
|};
  [%expect {| preserved |}]
;;

let%expect_test "inline preserves ignored argument exceptions" =
  preserves_runtime
    "Inline into uses"
    {|
let $f _ = 1
let () = assert (try ignore (f (failwith "expected")); false with Failure _ -> true)
|};
  [%expect {| preserved |}]
;;

let%expect_test "inline refuses partial application" =
  unavailable
    "Inline into uses"
    {|
let $f x y = x + y
let g = f 1
|};
  [%expect {| unavailable |}]
;;

let%expect_test "inline preserves labelled argument syntax" =
  preserves_runtime
    "Inline into uses"
    {|
let $x = 1
let f ~x = x + 1
let () = assert (f ~x = 2); assert (f ~x:x = 2)
|};
  [%expect {| preserved |}]
;;

let%expect_test "inline preserves optional argument syntax" =
  preserves_runtime
    "Inline into uses"
    {|
let $x = 1
let f ?(x = 0) () = x + 1
let () = assert (f ~x () = 2); assert (f ~x:x () = 2)
|};
  [%expect {| preserved |}]
;;

let%expect_test "inline refuses escaping function values" =
  unavailable
    "Inline into uses"
    {|
let $f x = x + 1
let same = f == f
|};
  [%expect {| unavailable |}]
;;

let%expect_test "inline refuses a partially safe transformation" =
  unavailable
    "Inline into uses"
    {|
let y = 1
let $f x = x + y
let good = f 1
let bad = let y = 2 in f 1
|};
  [%expect {| unavailable |}]
;;

let%expect_test "unsafe actions explain refusal to supporting clients" =
  let source, range =
    Code_actions.parse_selection "let f () = let $x = ref 0 in incr x; !x"
  in
  let codeAction = CodeActionClientCapabilities.create ~disabledSupport:true () in
  let textDocument = TextDocumentClientCapabilities.create ~codeAction () in
  let capabilities = ClientCapabilities.create ~textDocument () in
  Code_actions.iter_code_actions ~capabilities ~source range (fun actions ->
    let action =
      Option.value_exn actions
      |> List.find_map ~f:(function
        | `CodeAction ({ CodeAction.title = "Inline into uses"; _ } as action) ->
          Some action
        | _ -> None)
      |> Option.value_exn
    in
    assert (Option.is_none action.edit);
    print_endline (Option.value_exn action.disabled).reason);
  [%expect {| Inlining would repeat evaluation or allocation of the binding |}]
;;

