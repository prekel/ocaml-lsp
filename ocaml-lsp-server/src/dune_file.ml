open Import

type atom =
  { value : string
  ; first : int
  ; last : int
  }

type sexp =
  | Atom of atom
  | List of sexp list * bool

(* A small, location-preserving Dune reader. It never evaluates actions,
   variables or includes. Incomplete lists are useful for source navigation,
   but are never indexed as declarations. *)
let parse text =
  let length = String.length text in
  let cursor = ref 0 in
  let nodes = ref 0 in
  let exception Invalid in
  let whitespace = function
    | ' ' | '\t' | '\r' | '\n' | '\012' -> true
    | _ -> false
  in
  let rec skip () =
    if !cursor < length
    then (
      match text.[!cursor] with
      | ';' ->
        while !cursor < length && text.[!cursor] <> '\n' do
          incr cursor
        done;
        skip ()
      | c when whitespace c ->
        incr cursor;
        skip ()
      | _ -> ())
  in
  let rec sequence depth nested acc =
    skip ();
    if !cursor = length
    then List.rev acc, not nested
    else if text.[!cursor] = ')'
    then (
      incr cursor;
      if nested then List.rev acc, true else raise Invalid)
    else (
      incr nodes;
      if depth > 128 || !nodes > 100_000 then raise Invalid;
      let node =
        match text.[!cursor] with
        | '(' ->
          incr cursor;
          let items, complete = sequence (depth + 1) true [] in
          List (items, complete)
        | '"' ->
          incr cursor;
          let first = !cursor in
          if
            !cursor + 1 < length
            && text.[!cursor] = '\\'
            && (text.[!cursor + 1] = '|' || text.[!cursor + 1] = '>')
          then (
            (* End-of-line strings are opaque: parentheses in a shell script
               must not turn into Dune declarations or references. *)
            while !cursor < length && text.[!cursor] <> '\n' do
              incr cursor
            done;
            Atom { value = ""; first; last = !cursor })
          else (
            while !cursor < length && text.[!cursor] <> '"' do
              if text.[!cursor] = '\\' then incr cursor;
              if !cursor < length then incr cursor
            done;
            if !cursor = length then raise Invalid;
            let last = !cursor in
            incr cursor;
            let raw = String.sub text ~pos:first ~len:(last - first) in
            let value =
              try Scanf.unescaped raw with
              | Scanf.Scan_failure _ | Failure _ -> ""
            in
            Atom { value; first; last })
        | _ ->
          let first = !cursor in
          while
            !cursor < length
            && (not (whitespace text.[!cursor]))
            && not (List.mem [ '('; ')'; ';'; '"' ] text.[!cursor] ~equal:Char.equal)
          do
            incr cursor
          done;
          if !cursor = first then raise Invalid;
          Atom
            { value = String.sub text ~pos:first ~len:(!cursor - first)
            ; first
            ; last = !cursor
            }
      in
      sequence depth nested (node :: acc))
  in
  if length > 1_000_000
  then []
  else (
    try fst (sequence 0 false []) with
    | Invalid -> [])
;;

let static value =
  (not (String.is_empty value))
  && Option.is_none (String.substr_index value ~pattern:"%{")
  && not (Stdlib.String.contains value '\000')
;;

let field name fields =
  List.find_map fields ~f:(function
    | List (Atom key :: values, true) when String.equal key.value name -> Some values
    | _ -> None)
;;

let single = function
  | Some [ Atom atom ] when static atom.value -> Some atom
  | _ -> None
;;
