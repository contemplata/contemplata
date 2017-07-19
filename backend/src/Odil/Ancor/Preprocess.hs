-- | Pre-processing ancor file for parsing.


module Odil.Ancor.Preprocess
(
-- * Top-level
  prepare

-- * External
, Sym (..)
, RE
, mkRegexG
-- , mkRegex
, replace
-- ** High-level
, ExtConfig
, compile
, readConfig
) where


import Control.Monad (guard)
import Control.Applicative ((<|>))

import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Char as C

import qualified Text.Regex.Applicative as RE

import Odil.Ancor.Types
import qualified Odil.Ancor.IO.Parse as P
import qualified Odil.Ancor.IO.Show as S


---------------------------------------------------
-- Top-level preprocessing
---------------------------------------------------


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


---------------------------------------------------
-- External configuration: core
---------------------------------------------------


-- | A symbol for regex processing.
data Sym
  = Char Char
    -- ^ Just a regular character
  | Beg
    -- ^ Sentence beginning
  | End
    -- ^ Sentence ending
  deriving (Show, Eq, Ord)


-- | Retrieve the underlying character.
unChar :: Sym -> Maybe Char
unChar (Char x) = Just x
unChar _ = Nothing


-- | A regular expression.
type RE = RE.RE Sym [Sym]


-- | Replace (similar to `RE.replace`) the given RE (use `mkRegex` to construct
-- it) in the given string.
replace :: RE -> String -> String
replace re
  = mapMaybe unChar
  . RE.replace re
  . (Beg:) . (++[End])
  . map Char


-- | A version of `mkRegex` which deals with the surrounding spaces and the
-- special '^' character.
mkRegexG :: String -> RE
mkRegexG [] = pure []
mkRegexG (x : xs)
  | x == '^'  = glue <$> RE.sym Beg <*> (mkRegex xs *> break)
  | otherwise = glue <$> break <*> (mkRegex (x : xs) *> break)
  where
    break = RE.psym isSpace <|> RE.psym (`elem` [Beg, End])
    -- `glue` guarantees that an appropriate number of spaces is left in the
    -- resulting string
    glue (Char _) (Char _) = [Char ' ']
    glue _ _ = []


-- | Create a regular expression from a given string. The resulting value is
-- always an empty list (string), which corresponds to the fact that we want to
-- remove the recognized strings.
mkRegex :: String -> RE
mkRegex [] = pure []
mkRegex (x : xs)
  | C.isSpace x =
      RE.some (RE.psym isSpace) *> mkRegex xs
  | otherwise =
      RE.psym (char x) *> mkRegex xs


-- | `C.isSpace` lifted to symbols.
isSpace :: Sym -> Bool
isSpace (Char c) = C.isSpace c
isSpace _ = False


-- | Is the symbol equal (case insensitivelly) to the given character?
char :: Char -> Sym -> Bool
char x (Char c) = C.toLower x == C.toLower c
char _ _ = False


---------------------------------------------------
-- External configuration: high-level
---------------------------------------------------


-- | An external configuration is just a list of strings to be removed.
type ExtConfig = [String]


-- | Compile the configuration into a removal function.
compile :: ExtConfig -> String -> String
compile [] = id
compile (x:xs) =
  let re = mkRegexG x
  in  compile xs . replace re


-- | Read configuration from the given file.
readConfig :: FilePath -> IO ExtConfig
readConfig path = do
  xs <- map trim . lines <$> readFile path
  return $ filter (not . irrelevant) xs
  where
    trim = f . f
      where f = reverse . dropWhile C.isSpace
    irrelevant x = empty x || comment x
    empty = (=="")
    comment ('#' : _) = True
    comment _ = False
