type address =
  { city : string
  ; zip : int
  }


type t =
  { name : string
  ; age : int
  ; address : address
  ; tags : string list
  ; nickname : string option
  }

