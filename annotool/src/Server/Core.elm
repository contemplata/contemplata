module Server.Core exposing
    ( ParserTyp(..),  ParseReq(..)
    , encodeParseReq
    )


import Rose as R

-- import WebSocket
import Json.Decode as Decode
import Json.Encode as Encode
import WebSocket


---------------------------------------------------
-- Server websocket communication
---------------------------------------------------


-- | The type of parser to use.
type ParserTyp
  = Stanford
  | DiscoDOP


type ParseReq a
    = Single a
    | Batch (List a)


encodeParseReq : (a -> Encode.Value) -> ParseReq a -> Encode.Value
encodeParseReq encA parseReq =
    case parseReq of
        Single x ->
            Encode.object
                [ ("tag", Encode.string "Single")
                , ("contents", encA x)
                ]
        Batch xs ->
            Encode.object
                [ ("tag", Encode.string "Batch")
                , ("contents", Encode.list (List.map encA xs))
                ]
