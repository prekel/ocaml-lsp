(* Generated PPX expansion: a snapshot of the current editor buffer. *)

let rec address_to_yojson : address -> Yojson.Safe.t =
  ((let open! ((Ppx_deriving_yojson_runtime)[@ocaml.warning "-A"]) in
      fun x ->
        let fields = [] in
        let fields =
          ("zip", ((fun (x : Ppx_deriving_runtime.int) -> `Int x) x.zip)) ::
          fields in
        let fields =
          ("city",
            ((fun (x : Ppx_deriving_runtime.string) -> `String x) x.city))
          :: fields in
        `Assoc fields)
  [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
and address_of_yojson
  : Yojson.Safe.t -> address Ppx_deriving_yojson_runtime.error_or =
  ((let open! ((Ppx_deriving_yojson_runtime)[@ocaml.warning "-A"]) in
      function
      | `Assoc xs ->
          let rec loop xs ((arg0, arg1) as _state) =
            match xs with
            | ("city", x)::xs ->
                loop xs
                  ((Yojson_error.at_field "city"
                      ((function
                        | `String x -> Result.Ok x
                        | json -> Yojson_error.expected "string" json) x)),
                    arg1)
            | ("zip", x)::xs ->
                loop xs
                  (arg0,
                    (Yojson_error.at_field "zip"
                       ((function
                         | `Int x -> Result.Ok x
                         | json -> Yojson_error.expected "int" json) x)))
            | [] ->
                arg1 >>=
                  ((fun arg1 ->
                      arg0 >>=
                        (fun arg0 -> Result.Ok { city = arg0; zip = arg1 })))
            | (key, _)::xs -> Yojson_error.unknown_field key in
          loop xs
            ((Result.Error "$.city: missing required field \"city\""),
              (Result.Error "$.zip: missing required field \"zip\""))
      | json -> Yojson_error.expected "object" json)
  [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
let _ = address_to_yojson
let _ = address_of_yojson
