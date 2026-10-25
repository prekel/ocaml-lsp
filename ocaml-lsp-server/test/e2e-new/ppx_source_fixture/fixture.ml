(** The user's record, not the generated converter. *)
type address = { city : string } [@@deriving source_locations]

let read xs = source_address xs

module Nested = struct
  type t = { name : string } [@@deriving source_locations]

  let identity x = x
end
