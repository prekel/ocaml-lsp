open Import

(* TODO:

   - split the "needs merlin" part of the action out so that all actions can be
     batched *)

type t =
  { kind : CodeActionKind.t
  ; run :
      [ `Batchable of
          Mpipeline.t -> Document.t -> CodeActionParams.t -> CodeAction.t option
      | `Non_batchable of Document.t -> CodeActionParams.t -> CodeAction.t option Fiber.t
      ]
  }

let batchable kind run = { kind; run = `Batchable run }
let non_batchable kind run = { kind; run = `Non_batchable run }

let enclosing_nodes pipeline (range : Range.t) =
  let start = Mpipeline.get_lexing_pos pipeline (Position.logical range.start) in
  let browse =
    Mbrowse.of_typedtree (Mtyper.get_typedtree (Mpipeline.typer_result pipeline))
  in
  let nodes = Mbrowse.enclosing start [ browse ] in
  if Position.compare range.start range.end_ = Ordering.Eq
  then nodes
  else
    List.filter nodes ~f:(fun (_, node) ->
      let loc = Browse_raw.node_merlin_loc (Mbrowse.node_loc node) node in
      Option.exists (Range.of_loc_opt loc) ~f:(fun outer -> Range.contains outer range))
;;

let disabled ~kind ~title reason =
  CodeAction.create
    ~kind
    ~title
    ~isPreferred:false
    ~disabled:(CodeAction.create_disabled ~reason)
    ()
;;

let fresh_name doc base =
  let source = Msource.text (Document.source doc) in
  let rec loop i =
    let name = if i = 0 then base else sprintf "%s_%d" base i in
    let pattern = Re.compile (Re.seq [ Re.bow; Re.str name; Re.eow ]) in
    if Re.execp pattern source then loop (i + 1) else name
  in
  loop 0
;;

let has_comments pipeline (range : Range.t) =
  Mpipeline.reader_comments pipeline
  |> List.exists ~f:(fun (_, loc) ->
    match Range.of_loc_opt loc with
    | None -> true
    | Some comment ->
      Position.compare comment.start range.end_ = Ordering.Lt
      && Position.compare range.start comment.end_ = Ordering.Lt)
;;

let constructors_available (expr : Typedtree.expression) destination_env =
  let module I = Ocaml_typing.Tast_iterator in
  let exception Unavailable in
  let resolves_to env lid uid =
    match Env.find_constructor_by_name lid env with
    | constructor -> Types.Uid.equal uid constructor.cstr_uid
    | exception Not_found -> false
  in
  let check lid uid =
    if resolves_to expr.exp_env lid uid && not (resolves_to destination_env lid uid)
    then raise_notrace Unavailable
  in
  let expr_iter (iter : I.iterator) (expr : Typedtree.expression) =
    (match expr.exp_desc with
     | Texp_construct (lid, constructor, _) -> check lid.txt constructor.cstr_uid
     | _ -> ());
    I.default_iterator.expr iter expr
  in
  let pat_iter (type k) (iter : I.iterator) (pat : k Typedtree.general_pattern) =
    (match pat.pat_desc with
     | Tpat_construct (lid, constructor, _, _) -> check lid.txt constructor.cstr_uid
     | _ -> ());
    I.default_iterator.pat iter pat
  in
  let iterator = { I.default_iterator with expr = expr_iter; pat = pat_iter } in
  try
    iterator.expr iterator expr;
    true
  with
  | Unavailable -> false
;;

let relocatable_expression expr destination_env =
  let module I = Ocaml_typing.Tast_iterator in
  let portable = ref (constructors_available expr destination_env) in
  let expr_iter iter (e : Typedtree.expression) =
    (match e.exp_desc with
     | Texp_field _
     | Texp_record _
     | Texp_setfield _
     | Texp_instvar _
     | Texp_setinstvar _
     | Texp_letmodule _
     | Texp_object _
     | Texp_new _
     | Texp_pack _ -> portable := false
     | _ -> ());
    I.default_iterator.expr iter e
  in
  let typ iter (t : Typedtree.core_type) =
    (match t.ctyp_desc with
     | Ttyp_constr (path, { txt = lid; _ }, _) ->
       (match Env.find_type_by_name lid expr.exp_env with
        | original, _ when Path.same path original ->
          (match Env.find_type_by_name lid destination_env with
           | target, _ when Path.same path target -> ()
           | _ -> portable := false
           | exception Not_found -> portable := false)
        | _ -> ()
        | exception Not_found -> ())
     | Ttyp_package _ | Ttyp_class _ -> portable := false
     | _ -> ());
    I.default_iterator.typ iter t
  in
  let iter = { I.default_iterator with expr = expr_iter; typ } in
  iter.expr iter expr;
  !portable
;;
