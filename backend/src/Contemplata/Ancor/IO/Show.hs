{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}


module Contemplata.Ancor.IO.Show
  ( showTurn
  , showSection
  , showAncor
  , showToken
  , showElem

  , elem2sent
  ) where


import qualified Data.Text as T

import Contemplata.Ancor.Types
import qualified Contemplata.Server.Types as Contemplata


showAncor :: Episode -> T.Text
-- showAncor = T.intercalate "\n\n" . map showSection
showAncor = T.intercalate "\n" . filter (not . T.null . T.strip) . map showSection


-- | Show a given section.
showSection :: Section -> T.Text
showSection = T.intercalate "\n" . map showTurn
-- showSection = T.intercalate "\n" . filter (not . T.null . T.strip) . map showTurn


-- | Show a given turn.
showTurn :: Turn -> T.Text
showTurn Turn{..} = T.intercalate " " (map (showElem . snd) elems)


-- | Show a given element.
showElem :: Elem -> T.Text
showElem = T.unwords . map showToken


-- | Show a given chunk.
showToken :: Token -> T.Text
showToken = \case
  Plain x -> x
  Bruit x -> T.concat ["[", x ,"]"]
  Incomplete x y -> T.concat [x, "(", y ,")"]
  Pause x -> x
  Inaudible -> "[pi]"
  Pronounce x -> T.concat ["<", x ,">"]


---------------------------------------------------
-- Contemplata Sentence
---------------------------------------------------


-- | Show a given element.
elem2sent :: Elem -> Contemplata.Sent
elem2sent = map convertToken


-- | Convert Ancor token to the ODIL representation.
convertToken :: Token -> Contemplata.Token
convertToken tok = Contemplata.Token
  { Contemplata.orth = showToken tok
  , Contemplata.afterSpace = True }
