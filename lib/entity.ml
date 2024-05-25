[@@@warning "-30"]

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type function_ =
  { arguments : string
  ; name : string
  }
[@@deriving yojson, show, make]

type tool_call =
  { function_ : function_ [@key "function"]
  ; id : string
  ; type_ : string [@key "type"]
  }
[@@deriving yojson, show, make]

type message' =
  { content : string
  ; role : string
  ; tool_calls : tool_call list
  }
[@@deriving yojson, show, make]

type choice =
  { index : int
  ; message : message'
  ; finish_reason : string
  }
[@@deriving yojson, show, make]

type usage =
  { completion_tokens : int
  ; prompt_tokens : int
  ; total_tokens : int
  }
[@@deriving yojson, show, make]

type message =
  { choices : choice list
  ; created : int
  ; usage : usage
  }
[@@deriving yojson, show, make]
