{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}


-- | (Token-level) preprocessing ancor file for parsing.


module Odil.Ancor.Preprocess.Token
(
-- * Top-level
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

import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Char as C

import qualified Text.Regex.Applicative as RE

import Odil.Ancor.Types
import qualified Odil.Ancor.IO.Parse as P
import qualified Odil.Ancor.IO.Show as S

import qualified Odil.Server.Types as Odil
-- import qualified Odil.Ancor.Preprocess as Pre


---------------------------------------------------
-- Top-level preprocessing
---------------------------------------------------


-- | Perform pre-processing so as to:
-- * Join tokens (e.g. those which represent acronyms)
-- * Remove tokens irrelevant for parsing (represented as `Nothing` values in
--   the resulting list)
prepare
  :: ExtConfig
  -> [Odil.Token]
  -> [(Odil.Token, Maybe T.Text)]
prepare ext toks =
  -- backup . remove . prepareBase $ toks
  remove . prepareBase $ toks
  where
    remove = compile ext
--     backup xs =
--       if all ((==Nothing) . snd) xs
--       then prepareDummy toks
--       else xs


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


-- | A dummy preparation function which does nothing, really.
prepareDummy :: [Odil.Token] -> [(Odil.Token, Maybe T.Text)]
prepareDummy = map $ \x -> (x, Just $ Odil.orth x)


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
---------------------------------------------------
-- Removing expressions by parsing
---------------------------------------------------
---------------------------------------------------


-- | The expression to process.
data Expr = Expr
  { exprStr :: T.Text
  , onBeg :: Bool
    -- ^ On the beginning of the sentence
  }
-- type Expr = String



-- | Remove the expression from the list of tokens.
removeExpr
  :: Expr
  -> [(Odil.Token, Maybe T.Text)]
  -> [(Odil.Token, Maybe T.Text)]
removeExpr Expr{..} =
  if onBeg
  then removeOne exprStr
  else removeAll exprStr


-- | Remove the expression only from the beginning of the list of tokens.
removeOne
  :: T.Text
  -> [(Odil.Token, Maybe T.Text)]
  -> [(Odil.Token, Maybe T.Text)]
removeOne str0 lst =

  let (xs, ys) = spanAcc exprElem str0 lst
  in  case xs of
        [] -> ys
        _  -> remExpr xs ++ ys

  where

    exprElem :: T.Text -> (Odil.Token, Maybe T.Text) -> (T.Text, Bool)
    exprElem str (odilTok, mayTok) = maybe (str, False) id $ do
      -- guard . not $ T.null str
      tok <- mayTok
      -- Just suff <- pure $ stripPrefixNS tok str
      -- return (suff, True)
      Just suff <- pure $ T.stripPrefix tok str
      return (T.strip suff, True)

    remExpr :: [(Odil.Token, Maybe T.Text)] -> [(Odil.Token, Maybe T.Text)]
    remExpr = map $ Arr.second (const Nothing)


-- | Remove the expression starting from all positions in the list of tokens.
removeAll
  :: T.Text
  -> [(Odil.Token, Maybe T.Text)]
  -> [(Odil.Token, Maybe T.Text)]
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


-- | Compile the configuration into a removal function (one-step).
compile
  :: ExtConfig
  -> [(Odil.Token, Maybe T.Text)]
  -> [(Odil.Token, Maybe T.Text)]
compile [] = id
compile (x:xs) = compile xs . removeExpr x


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


-- | A bit list `List.span` but with accumulator.
spanAcc :: (acc -> a -> (acc, Bool)) -> acc -> [a] -> ([a], [a])
spanAcc f =
  go
  where
    go acc xs = case xs of
      [] -> ([], [])
      hd : tl ->
        let (newAcc, sat) = f acc hd
        in
          if sat
          then Arr.first (hd:) (go newAcc tl)
          else ([], xs)


-- | Apply the function on tail.
onTail :: ([a] -> [a]) -> [a] -> [a]
onTail f = \case
  [] -> []
  x : xs -> x : f xs


-- -- | A bit like `T.stripPrefix` but ignores spaces.
-- stripPrefixNS
--   :: T.Text
--      -- ^ The prefix
--   -> T.Text
--      -- ^ The text to remove the prefix in
--   -> Maybe T.Text
--      -- ^ The remaining suffix
-- stripPrefixNS pref txt =
--   go
--   ( pref)
--   (T.filter (not . C.isSpace) txt)
--   where
--     prepare = T.filter (not . C.isSpace)
