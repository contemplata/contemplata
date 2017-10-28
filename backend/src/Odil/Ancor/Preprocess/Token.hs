-- | (Token-level) preprocessing ancor file for parsing.


module Odil.Ancor.Preprocess.Token
(
-- * Top-level
  prepare

-- * External
-- ** High-level
, Pre.ExtConfig
, Pre.readConfig
) where


import Control.Monad (guard)
import Control.Applicative ((<|>))
import qualified Control.Arrow as Arr

import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Char as C

import qualified Text.Regex.Applicative as RE

import Odil.Ancor.Types
import qualified Odil.Ancor.IO.Parse as P
import qualified Odil.Ancor.IO.Show as S

import qualified Odil.Server.Types as Odil
import qualified Odil.Ancor.Preprocess as Pre


---------------------------------------------------
-- Top-level preprocessing
---------------------------------------------------


-- | Perform pre-processing so as to:
-- * Join tokens (e.g. those which represent acronyms)
-- * Remove tokens irrelevant for parsing (represented as `Nothing` values in
--   the resulting list)
prepare
  :: Pre.ExtConfig
  -> [Odil.Token]
  -> [(Odil.Token, Maybe T.Text)]
prepare _ext toks =
  backup . prepareBase $ toks
  where
    backup xs =
      if all ((==Nothing) . snd) xs
      then map (\x -> (x, Just $ Odil.orth x)) toks
      else xs


-- | Prepare a given sentence for parsing.
prepareBase
  :: [Odil.Token]
  -> [(Odil.Token, Maybe T.Text)]
prepareBase
  = map (Arr.second $ fmap S.showToken)
  . retokenize
  . map (Arr.second Just)
  . map (Arr.second P.parseToken)
  . map (\x -> (x, Odil.orth x))
  where
    retokenize
      = map (Arr.second $ fmap complete)
      . rmBruit
      . rmPause
      . rmInaudible
      . rmPronounce
      . joinAcronyms


---------------------------------------------------
-- Re-tokenization
---------------------------------------------------


-- | A single re-tokenization strategy.
type Retok
  =  [(Odil.Token, Maybe Token)]
  -> [(Odil.Token, Maybe Token)]


-- | Remove bruits.
rmBruit :: Retok
rmBruit = remove isBruit


-- | Remove bruits.
rmPause :: Retok
rmPause = remove isPause


-- | Remove inaudible parts.
rmInaudible :: Retok
rmInaudible = remove isInaudible


-- | Remove "pronounce"(?) parts.
rmPronounce :: Retok
rmPronounce = remove isPronounce


joinAcronyms :: Retok
joinAcronyms lst =
  let (xs, ys) = L.span acroElem lst
  in  case xs of
        [] -> onTail joinAcronyms ys
        _  -> mkAcro xs : joinAcronyms ys
  where
    acroElem (odilTok, mayTok) = maybe False id $ do
      tok <- mayTok
      Plain x <- pure tok
      [c] <- pure (T.unpack x)
      guard $ C.isUpper c
      return True
    onTail f xs = case xs of
      hd : tl -> hd : f tl
      [] -> []
    mkAcro =
      let onSecond = Just . Plain . T.concat . mapMaybe extract
      in  Arr.first mconcat . Arr.second onSecond . unzip
    extract tok = case tok of
      Just (Plain x) -> Just x
      _ -> Nothing


---------------------------------------------------
-- Utils
---------------------------------------------------


isBruit :: Token -> Bool
isBruit x = case x of
  Bruit _ -> True
  _ -> False


isPause :: Token -> Bool
isPause x = case x of
  Pause _ -> True
  _ -> False


isInaudible :: Token -> Bool
isInaudible x = case x of
  Inaudible -> True
  _ -> False


-- | Complete incomplete tokens.
complete :: Token -> Token
complete x = case x of
  Incomplete x y -> Plain (x `T.append` y)
  _ -> x


isPronounce :: Token -> Bool
isPronounce x = case x of
  Pronounce _ -> True
  _ -> False


-- | Replace elements which satisfy the predicate with `Nothing.`
remove :: (a -> Bool) -> [(b, Maybe a)] -> [(b, Maybe a)]
remove p =
  map (Arr.second f)
  where
    f Nothing = Nothing
    f (Just x) =
      if p x
      then Nothing
      else Just x
