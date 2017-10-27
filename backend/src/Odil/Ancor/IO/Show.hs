{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}


module Odil.Ancor.IO.Show
  ( showTurn
  , showSection
  , showAncor
  , showToken
  , showElem

  , elem2sent
  ) where


import qualified Data.Text as T

import Odil.Ancor.Types
import qualified Odil.Server.Types as Odil


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
-- Odil Sentence
---------------------------------------------------


-- | Show a given element.
elem2sent :: Elem -> Odil.Sent
elem2sent = map convertToken


-- | Convert Ancor token to the ODIL representation.
convertToken :: Token -> Odil.Token
convertToken tok = Odil.Token
  { Odil.orth = showToken tok
  , Odil.afterSpace = True }
