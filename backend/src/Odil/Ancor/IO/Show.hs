{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Odil.Ancor.IO.Show (showTurn, showSection, showAncor) where


import qualified Data.Text as T

import Odil.Ancor.Types


showAncor :: Episode -> T.Text
showAncor = T.intercalate "\n\n" . map showSection


-- | Show a given section.
showSection :: Section -> T.Text
showSection = T.intercalate "\n" . map showTurn


-- | Show a given turn.
showTurn :: Turn -> T.Text
showTurn Turn{..} = T.intercalate " " (map showElem elems)


-- | Show a given element.
showElem :: Elem -> T.Text
showElem = unElem
