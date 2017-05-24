-- | Working with the Penn format.


module Odil.Penn
( Tree
, parseTree
, parseForest
, parseFile
, dividePenn

-- * Conversion
, toOdilTree
, convertPennFile
) where


import Control.Monad (void)
import Control.Applicative ((<|>))
import qualified Control.Arrow as Arr
import qualified Control.Monad.Trans.State.Strict as ST

import qualified Data.Attoparsec.Text as A
import qualified Data.Tree as R
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Traversable as Trav
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Char as C
import qualified Data.List as L


import qualified Odil.Server.Types as Odil


---------------------------------------------------
-- Types
---------------------------------------------------


-- | A Penn tree.
type Tree = R.Tree T.Text


---------------------------------------------------
-- Conversion
---------------------------------------------------


-- | Conversion state.
data Conv = Conv
  { idAcc :: Int
  , posAcc :: Int }


-- | Conver a given Penn tree to an ODIL tree.
toOdilTree :: Tree -> Odil.Tree
toOdilTree =

  flip ST.evalState
    (Conv 0 0)
  . Trav.traverse f
  . markLeaves

  where

    f (x, isLeaf) =
      if isLeaf then do
        i <- newId
        p <- newPos
        return $ Odil.Leaf i x p
      else do
        i <- newId
        return $ Odil.Node i x

    markLeaves (R.Node x ts) = case ts of
      [] -> R.Node (x, True) []
      _  -> R.Node (x, False) (map markLeaves ts)

    newId = do
      conv <- ST.get
      ST.put $ conv {idAcc = idAcc conv + 1}
      return $ idAcc conv

    newPos = do
      conv <- ST.get
      ST.put $ conv {posAcc = posAcc conv + 1}
      return $ posAcc conv


-- | Convert a list of pairs, each pair consisting of an (i) original sentence
-- and the (ii) corresponding tree, to an Odil file.
convertPennFile :: [(T.Text, Tree)] -> Odil.File
convertPennFile
  = flip Odil.File S.empty
  . M.fromList
  -- . zip (map (T.pack . show) [1..])
  . zip [1..]
  . map (Arr.second toOdilTree)
  -- . parseForest


---------------------------------------------------
---------------------------------------------------
-- Parsing
---------------------------------------------------
---------------------------------------------------


parseTree :: T.Text -> Tree
parseTree x =
  case A.parseOnly (treeP <* A.endOfInput) x of
    Left err -> error err
    Right t -> t


parseForest :: T.Text -> [Tree]
parseForest = map parseTree . dividePenn


-- | Divide the Penn file into a list of tree-related textual chunks.
dividePenn :: T.Text -> [T.Text]
dividePenn
  = filter (not . T.null)
  . map (T.strip . T.unlines)
  . L.groupBy
    (\_ line -> not $ T.null line)
  . map T.strip
  . T.lines


parseFile :: FilePath -> IO [Tree]
parseFile filePath = do
  cs <- T.readFile filePath
  return $ parseForest cs


---------------------------------------------------
-- Parsing combinators
---------------------------------------------------


-- | A tree parser.
treeP :: A.Parser Tree
treeP = nodeP <|> leafP


-- | A tree parser.
nodeP :: A.Parser Tree
nodeP = between (A.char '(') (A.char ')') $ do
  x <- labelP
  spaces
  ts <- treeP `A.sepBy1` spaces
  return $ R.Node x ts


-- | A root label parser.
labelP :: A.Parser T.Text
labelP = A.takeTill $ \c ->
  C.isSpace c || c == ')' || c == '('


-- | A leaf parser.
leafP :: A.Parser Tree
leafP = flip R.Node [] <$> labelP


---------------------------------------------------
-- Utils
---------------------------------------------------


between :: A.Parser a -> A.Parser b -> A.Parser c -> A.Parser c
between p q main = do
  void p
  x <- main
  void q
  return x


spaces :: A.Parser ()
spaces = A.skipMany1 A.space
