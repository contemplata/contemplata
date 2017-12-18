{- We use Dhall to generate the main annotation config file because, among
others, it allows comments... Later on we might use some more powerful language
features, but beware that what we send to the client side (Elm) must be a
regular JSON anyway. -}

{ -- What types of node annotations are allowed
  entities =
    [ ./Anno/timex.val
    , ./Anno/event.val
    , ./Anno/signal.val
    ] : List ./Anno/Entity.typ
}
