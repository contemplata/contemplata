-- | Entry page: Contemplata waits for the server to send the configuration and
-- the files to annotate. Once this is done, Contemplata enters the actuall file
-- editing mode.


module Entry exposing
  (
  -- Model
    Model
  , setAnnoConfig

  -- Messages
  , Msg(..), update

  -- View
  , view

  -- Subscriptions
  , subscriptions

  -- Initialization
  , mkEntry
  )


import Html as Html
import Html.Attributes as Atts
import Html.Events as Events
import Json.Decode as Decode
import Json.Encode as Encode
import Dict as D

import Util exposing ((:>))
import Config as Cfg
import Edit.Config
import Rose as R
import Edit.Core as C
import Server


---------------------------------------------------
-- Model
---------------------------------------------------


type alias Model =
  { config : Cfg.Config
  , annoConfig : Maybe Edit.Config.Config
  , fileIds : List C.FileId
  }


-- | Set the annotation configuration, and, once this is done, set the request
-- to fetch the files.
setAnnoConfig : Edit.Config.Config -> Model -> (Model, Cmd Msg)
setAnnoConfig annoCfg model =
    ( {model | annoConfig = Just (Debug.log "annoCfg" annoCfg)}
    , Server.sendWS model.config (Server.GetFiles model.config.user model.fileIds)
    )


---------------------------------------------------
-- Messages
---------------------------------------------------


-- | A dummy message.
type Msg = Msg


-- | Nothing really happens.
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)


---------------------------------------------------
-- View
---------------------------------------------------


-- The function responsible for viewing the entry page.
view : Model -> Html.Html Msg
view model =
  Html.div
    [ Atts.style
        [ "position" :> "absolute"
        , "left" :> "50%"
        , "top" :> "50%"
        , "-webkit-transform" :> "translate(-50%, -50%)"
        , "transform" :> "translate(-50%, -50%)"
        ]
    ]
    [ Html.text <|
          case model.fileIds of
              [] -> "Incorrent file IDs"
              _  -> case model.annoConfig of
                  Nothing  -> "Fetching annotation configuration"
                  Just cfg -> "Fetching " ++
                      String.join ", " (List.map C.encodeFileId model.fileIds)
    ]


---------------------------------------------------
-- Subscriptions
---------------------------------------------------


-- | Actually, no subscriptions at all.
subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


---------------------------------------------------
-- Initialization
---------------------------------------------------


-- | Initialize the entry with the given configuration.
mkEntry
    : Cfg.Config
    -> List C.FileId
    -> (Model, Cmd Msg)
mkEntry config fileIds =
  let
    model = {config=config, annoConfig=Nothing, fileIds=fileIds}
    init =
        case fileIds of
            [] -> Cmd.none
            _ -> Server.sendWS config Server.GetConfig
  in
    (model, init)
