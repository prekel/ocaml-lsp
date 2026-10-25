type a = { first : string }
and b = { second : string } [@@deriving source_locations, second]

module Nested = struct
  type t = { name : string } [@@deriving source_locations, second]
end
