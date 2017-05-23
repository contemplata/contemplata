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


type TextType a = Plain a | Bold a


viewHelp : Html.Html Msg
viewHelp =
  let
    cmdListTxt =
      [ [Plain "Menu commands: click ", Bold "Save", Plain " to save the current file and ", Bold "Menu", Plain " to return here."]
      , [ Plain "Tree selection: use ", Bold "page down", Plain " or ", Bold "page up", Plain " to go to the next or the previous tree, respectively. "
        , Plain "You can also click on the sentence identifiers in the side window on the right." ]
      , [Plain "Node selection: use the", Bold " left mouse button", Plain " to select the main node, and the ", Bold "left mouse button + CTRL", Plain " to select auxiliary nodes.  Selection works in a window-specific manner."]
      , [Plain "Add node: select node(s) and press ", Bold "a", Plain "."]
      , [Plain "Delete node: select node(s) and press ", Bold "d", Plain "."]
      , [Plain "Edit node: select the main node and press ", Bold "e", Plain ", which should bring you to the editing window."]
      , [Plain "Re-attach tree: select the root of the tree to move, then select (with ", Bold "CTRL", Plain ") the node where the tree should be re-attached, and press ", Bold "r", Plain "."]
      , [Plain "Swap tree: select the root of the tree to swap with its left or right sister tree and press ", Bold "CTRL + left", Plain " or ", Bold "CTRL + right", Plain "respectively."]
      , [Plain "Connect: to connect two nodes, select the main node in the top window and in the bottom-window, respectively, and press ", Bold "c", Plain ". The relation will lead to the node in the currently focused window."]
      , [Plain "Resize: you can use the regular browser's functionality to zoom in and out."]
      , [Plain "Screen size: use arrows (", Bold "left, right, top, bottom", Plain ") to change the size of the screens."]
      ]
    -- mkElem x = Html.li [] [Html.text x]
    mkCmd x = case x of
      Plain txt -> Html.text txt
      Bold txt -> Html.b [] [Html.text txt]
    mkElem x = Html.li [] [Html.span [] (List.map mkCmd x)]
    cmdList = Html.ul [] (List.map mkElem cmdListTxt)

    sideListTxt =
      [ [Plain "Edit: editing window (not much functionality for the moment)."]
      , [Plain "Context: shows the sentences in the current file."]
      , [Plain "Messages: messages received from the server (with e.g. information that the file has been successfully saved)."]
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
