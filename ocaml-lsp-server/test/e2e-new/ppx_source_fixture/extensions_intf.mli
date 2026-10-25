[%%source_locations.signature_item]

module M : [%source_locations.mod_intf]

module type S = [%source_locations.mty_intf]

type alias = [%source_locations.typ_intf]
type record = { label : string } [@@source_locations]

class type ct = [%source_locations.cty_intf]

[@@@source_locations.floating]

type event = ..
type event += Extra [@@deriving source_locations]
