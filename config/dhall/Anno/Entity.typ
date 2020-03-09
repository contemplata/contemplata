{ name : Text
, typ : ./EntityType.typ
, attributes : List {name : Text, value : ./Attr.typ}
, attributesOnType : List 
    { typ : Text
    , attributes : List {name : Text, value : ./Attr.typ} }
}
