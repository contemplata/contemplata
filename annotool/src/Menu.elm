module Menu exposing
  (
  -- Model
    Model
  -- Messages
  , Msg(..), update
  -- View
  , view
  -- Subscriptions
  , subscriptions
  -- Server communication
  -- , Request(..) , Answer (..), answerDecoder, encodeReq
  -- Initialization
  , mkMenu
  )


import Html as Html
import Html.Attributes as Atts
import Html.Events as Events
import WebSocket
import Json.Decode as Decode
import Json.Encode as Encode
import Dict as D

import Util
import Config as Cfg
import Rose as R
import Edit.Model as M
import Server


---------------------------------------------------
-- Model
---------------------------------------------------


type alias Model =
  { fileIds : List M.FileId }


---------------------------------------------------
-- Messages
---------------------------------------------------


type Msg
  = Choice M.FileId -- ^ Edit a specific file
  | ShowFiles (List M.FileId)
--   | ServerMsg Answer -- ^ Get message from the websocket
--   | Error String  -- ^ An error message


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let idle x = (x, Cmd.none) in
  case msg of
    Choice fileId ->
      let cmd = WebSocket.send Cfg.socketServer <|
                  Server.encodeReq (Server.GetFile fileId)
      in  (model, cmd)
    ShowFiles ids -> idle <| {model | fileIds = ids}


---------------------------------------------------
-- View
---------------------------------------------------


view : Model -> Html.Html Msg
view model =
  Html.div
    [ Atts.style
        [ "width" => "100%"
        , "height" => "100%"
        , "padding" => Util.px Cfg.menuPadding
        , "margin" => "auto"
        , "max-width" => Util.px Cfg.menuMaxWidth
        ]
    ]
    [ viewFiles model
    , viewHelp
    ]


viewFiles : Model -> Html.Html Msg
viewFiles model =
  let
    list = Html.ul []
      (List.map viewFileId model.fileIds)
  in
    Html.div
      [ Atts.style
          [ "float" => "left"
          , "max-width" => Util.px Cfg.menuFilesMaxWidth
          , "margin-right" => Util.px Cfg.menuHelpMargin
          -- , "width" => "50%"
          -- , "left" => Util.px 0
          -- , "height" => "100%"
          ]
      ]
      [ Html.h3 [] [Html.text "Files"]
      , list
      ]


viewFileId : M.FileId -> Html.Html Msg
-- viewFileId x = Html.li [] <| Util.single <|
--   Html.div
--     [ Atts.class "noselect"
--     , Events.onClick (Choice x)
--     , Atts.style ["cursor" => "pointer"]
--     ]
--     [Html.text x]
viewFileId x = Html.li
  [ Events.onClick (Choice x)
  -- , Atts.class "noselect"
  , Atts.style ["cursor" => "pointer"]
  ]
  [Html.text x]


viewHelp : Html.Html Msg
viewHelp =
  let
    cmdListTxt =
      [ "Menu commands: click 'Save' to save the current file and 'Menu' to return here."
      , "Selection: use the left mouse button to select the main node, and the left mouse button + CTRL to select auxiliary nodes.  Selection works in a window-specific manner."
      , "Add node: select node(s) and press 'a'."
      , "Delete node: select node(s) and press 'd'."
      , "Edit node: select the main node and press 'e', which should bring you to the editing window."
      , "Re-attach tree: select the root of thre tree to move, then select (with CTRL) the node where the tree should be re-attached, and press 'r'."
      , "Connect: to connect two nodes, select the main node in the top window and in the bottom-window, respectively, and press 'c'.  The relation will lead to the node in the currently focused window."
      , "Resize: you can use the regular browser's functionality to zoom in and out."
      , "Screen size: use arrows (left, right, top, bottom) to change the size of the screens."
      ]
    mkElem x = Html.li [] [Html.text x]
    cmdList = Html.ul [] (List.map mkElem cmdListTxt)

    sideListTxt =
      [ "Edit: editing window (not much functionality for the moment)."
      , "Context: shows the sentences in the current file."
      , "Messages: messages received from the server (with e.g. information the file has been successfully saved)."
      ]
    sideList = Html.ul [] (List.map mkElem sideListTxt)
  in
    Html.div
      [ Atts.style
          [ "float" => "left"
          , "max-width" => Util.px Cfg.menuHelpMaxWidth
          -- , "width" => "50%"
          -- , "right" => Util.px 0
          -- , "height" => "100%"
          ]
      ]
      [ Html.h3 [] [Html.text "Help"]
      , Html.p [] [Html.text "Once you select one of the files on the left, you will be able to use the following commands: "]
      , cmdList
      , Html.p [] [Html.text "Note that in each of the two side windows you can choose between:"]
      , sideList
      ]


---------------------------------------------------
-- Subscriptions
---------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


---------------------------------------------------
-- Initialization
---------------------------------------------------


mkMenu : (Model, Cmd Msg)
mkMenu =
  let
    model = {fileIds = []}
    init = WebSocket.send Cfg.socketServer (Server.encodeReq Server.GetFiles)
  in
    (model, init)


---------------------------------------------------
-- Utils
---------------------------------------------------


(=>) : a -> b -> (a, b)
(=>) = (,)
