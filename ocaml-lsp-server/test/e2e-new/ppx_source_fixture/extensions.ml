let expression = [%source_locations.expr]
let attributed = (1 [@source_locations])
let [%source_locations.pat] = 2
let _ = captured

type alias = [%source_locations.typ]

module M = [%source_locations.module_expr]

module type S = [%source_locations.mty]

[%%source_locations.str]

let%source_locations.sugar sugar = 3

type record = { label : string } [@@source_locations]

class c = [%source_locations.cls]

class type ct = [%source_locations.cty]

[@@@source_locations.floating]

type event = ..
type event += Extra [@@deriving source_locations]
