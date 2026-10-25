type address =
  { city : string
  ; zip : int
  }
[@@deriving yojson]

type t =
  { name : string
  ; age : int
  ; address : address
  ; tags : string list
  ; nickname : string option
  }
[@@deriving yojson]
