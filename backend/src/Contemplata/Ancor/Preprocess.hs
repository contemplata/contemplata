{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}


-- | Token-level ANCOR preprocessing, with parsing in mind. See the `prepare`
-- function.


module Contemplata.Ancor.Preprocess
(
-- * Preprocessing
  prepare
, prepareDummy

-- * External
, ExtConfig
, Expr (..)
, readConfig
) where


import Control.Monad (guard)
import Control.Applicative ((<|>))
import qualified Control.Arrow as Arr

import Data.Maybe (mapMaybe, isNothing)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Char as C

import qualified Text.Regex.Applicative as RE

import Contemplata.Ancor.Types
import qualified Contemplata.Ancor.IO.Parse as P
import qualified Contemplata.Ancor.IO.Show as S

import qualified Contemplata.Types as Contemplata


---------------------------------------------------
-- Preprocessing
---------------------------------------------------


-- | Perform pre-processing so as to:
-- * Join tokens (e.g. those which represent acronyms)
-- * Remove tokens irrelevant for parsing (represented as `Nothing` values in
--   the resulting list)
prepare
  :: ExtConfig
  -> [Contemplata.Token]
  -> [(Contemplata.Token, Maybe T.Text)]
prepare ext toks =
  remove . prepareBase $ toks
  where
    remove = compile ext


-- | Prepare a given sentence for parsing.
prepareBase
  :: [Contemplata.Token]
  -> [(Contemplata.Token, Maybe T.Text)]
prepareBase
  = map (Arr.second $ fmap S.showToken)
  . retokenize
  . map (Arr.second Just)
  . map (Arr.second P.parseToken)
  . map (\x -> (x, Contemplata.orth x))
  where
    retokenize
      = map (Arr.second $ fmap complete)
      . rmBruit
      . rmPause
      . rmInaudible
      . rmPronounce
      . joinAcronyms


-- | A dummy preparation function which does nothing, really.
prepareDummy :: [Contemplata.Token] -> [(Contemplata.Token, Maybe T.Text)]
prepareDummy = map $ \x -> (x, Just $ Contemplata.orth x)


---------------------------------------------------
-- Re-tokenization
---------------------------------------------------


-- | A single re-tokenization strategy.
type Retok
  =  [(Contemplata.Token, Maybe Token)]
  -> [(Contemplata.Token, Maybe Token)]


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
---------------------------------------------------
-- Removing expressions by parsing
---------------------------------------------------
---------------------------------------------------


-- | An expression to match (and remove)
data Expr = Expr
  { exprStr :: T.Text
    -- ^ The actual expression
  , onBeg :: Bool
    -- ^ Matches only at the beginning of a sentence
  }


-- | Remove the given expression from the list of tokens.
removeExpr
  :: Expr
  -> [(Contemplata.Token, Maybe T.Text)]
  -> [(Contemplata.Token, Maybe T.Text)]
removeExpr Expr{..} =
  if onBeg
  then uncurry (++)
    . Arr.second (removeOne exprStr)
    . L.span (isNothing . snd)
  else removeAll exprStr


-- | Remove the expression *only* from the beginning of the list of tokens.
removeOne
  :: T.Text
  -> [(Contemplata.Token, Maybe T.Text)]
  -> [(Contemplata.Token, Maybe T.Text)]
removeOne str0 lst =

  let (strRest, (xs, ys)) = spanAcc exprElem str0 lst
  in  case xs of
        [] -> ys
        _ | not (T.null strRest) -> xs ++ ys
        _  -> remExpr xs ++ ys

  where

    exprElem :: T.Text -> (Contemplata.Token, Maybe T.Text) -> (T.Text, Bool)
    exprElem str (odilTok, mayTok) = maybe (str, False) id $ do
      tok <- mayTok
      Just suff <- pure $ T.stripPrefix tok str
      return (T.strip suff, True)

    remExpr :: [(Contemplata.Token, Maybe T.Text)] -> [(Contemplata.Token, Maybe T.Text)]
    remExpr = map $ Arr.second (const Nothing)


-- | Remove the expression starting from all positions in the list of tokens.
-- TODO: It seems to work incorrectly when the expressions to remove are
-- adjacent!
removeAll
  :: T.Text
  -> [(Contemplata.Token, Maybe T.Text)]
  -> [(Contemplata.Token, Maybe T.Text)]
removeAll str = onTail (removeAll str) . removeOne str


---------------------------------------------------
-- External configuration
---------------------------------------------------


-- | Parse the expression from its string representation.
parseExpr :: String -> Expr
parseExpr str = case str of
  '^' : rest -> Expr (T.pack rest) True
  _ -> Expr (T.pack str) False


-- | An external configuration is just a list of strings to be removed.
type ExtConfig = [Expr]


-- | Read configuration from the given file.
readConfig :: FilePath -> IO ExtConfig
readConfig path = do
  xs <- map trim . lines <$> readFile path
  return . map parseExpr . filter (not . irrelevant) $ xs
  where
    trim = f . f
      where f = reverse . dropWhile C.isSpace
    irrelevant x = empty x || comment x
    empty = (=="")
    comment ('#' : _) = True
    comment _ = False


-- | Compile the configuration into a removal function (`step` as long as the
-- result differs from the input).
compile
  :: ExtConfig
  -> [(Contemplata.Token, Maybe T.Text)]
  -> [(Contemplata.Token, Maybe T.Text)]
compile cfg =
  let
    once = step cfg
    go toks =
      let toks' = once toks
      in  if toks == toks'
          then toks
          else go toks'
  in
    go


-- | Compile the configuration into a removal function (one-step).
step
  :: ExtConfig
  -> [(Contemplata.Token, Maybe T.Text)]
  -> [(Contemplata.Token, Maybe T.Text)]
step [] = id
step (x:xs) = step xs . removeExpr x


---------------------------------------------------
---------------------------------------------------
-- Utils
---------------------------------------------------
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


-- | A bit like `List.span` but with an accumulator.
spanAcc :: (acc -> a -> (acc, Bool)) -> acc -> [a] -> (acc, ([a], [a]))
spanAcc f =
  go
  where
    go acc xs = case xs of
      [] -> (acc, ([], []))
      hd : tl ->
        let (newAcc, sat) = f acc hd
        in
          if sat
          then Arr.second (Arr.first (hd:)) (go newAcc tl)
          else (acc, ([], xs))


-- | Apply the function on tail.
onTail :: ([a] -> [a]) -> [a] -> [a]
onTail f = \case
  [] -> []
  x : xs -> x : f xs
