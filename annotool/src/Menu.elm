module Menu exposing
  (
  -- | Model
    Model, FileId
  -- | Messages
  , Msg, update
  -- | View
  , view
  -- | Subscriptions
  , subscriptions
  )


import Html as Html
import Html.Attributes as Atts
import Html.Events as Events


---------------------------------------------------
-- Model
---------------------------------------------------


type alias Model =
  { fileIds : List FileId }


type alias FileId = String


---------------------------------------------------
-- Messages
---------------------------------------------------


type Msg
  = Choice FileId -- ^ Edit a specific file


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Choice fileId -> (model, Cmd.none)


---------------------------------------------------
-- View
---------------------------------------------------


view : Model -> Html.Html Msg
view model =
  Html.div
    [ Atts.style
        [ "width" => "100%"
        , "height" => "100%"
        ]
    ]
    [ Html.ul []
        (List.map viewFileId model.fileIds)
    ]


viewFileId : FileId -> Html.Html Msg
viewFileId x = Html.li [] [Html.text x]


---------------------------------------------------
-- Subscriptions
---------------------------------------------------


subscriptions : Model -> Sub Msg
-- subscriptions = Debug.crash "Menu.subs"
subscriptions _ = Sub.none


---------------------------------------------------
-- Utils
---------------------------------------------------


(=>) : a -> b -> (a, b)
(=>) = (,)
