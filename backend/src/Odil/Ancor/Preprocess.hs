-- | Pre-processing ancor file for parsing.


module Odil.Ancor.Preprocess (prepare) where


import Control.Monad (guard)

import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Char as C

import Odil.Ancor.Types
import qualified Odil.Ancor.IO.Parse as P
import qualified Odil.Ancor.IO.Show as S


-- | Prepare a given sentence for parsing.
prepare :: T.Text -> T.Text
prepare
  = T.unwords
  . map S.showToken
  . retokenize
  . map P.parseToken
  . T.words
  where
    retokenize sent
      = backup sent
      . map complete
      . rmBruit
      . rmPause
      -- . rmInaudible
      . joinAcronyms
      $ sent
    backup sent xs =
      if null xs
      then sent
      else xs


---------------------------------------------------
-- Re-tokenization
---------------------------------------------------


-- | A single re-tokenization strategy.
type Retok = [Token] -> [Token]


-- -- | Combine several re-tokenization strategies.
-- retokenize :: [Retok] -> [Token] -> [Token]
-- retokenize = foldl (.) id


-- | Remove bruits.
rmBruit :: Retok
rmBruit = filter (not . isBruit)


-- | Remove bruits.
rmPause :: Retok
rmPause = filter (not . isPause)


-- | Remove inaudible parts.
rmInaudible :: Retok
rmInaudible = filter (not . isInaudible)


joinAcronyms :: Retok
joinAcronyms lst =
  let (xs, ys) = L.span acroElem lst
  in  case xs of
        [] -> onTail joinAcronyms ys
        _  -> mkAcro xs : joinAcronyms ys
  where
    onTail f xs = case xs of
      hd : tl -> hd : f tl
      [] -> []
    acroElem tok = maybe False id $ do
      Plain x <- pure tok
      [c] <- pure (T.unpack x)
      guard $ C.isUpper c
      return True
    mkAcro = Plain . T.concat . mapMaybe extract
    extract tok = case tok of
      Plain x -> Just x
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

