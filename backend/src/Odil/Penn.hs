-- | Working with the Penn format.


module Odil.Penn
( Tree
, parseTree
, parseForest
, parseFile
, dividePenn
) where


import Control.Monad (void)
import Control.Applicative ((<|>))

import qualified Data.Attoparsec.Text as A
import qualified Data.Tree as R
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Char as C
import qualified Data.List as L


---------------------------------------------------
-- Types
---------------------------------------------------


-- | A Penn tree.
type Tree = R.Tree T.Text


---------------------------------------------------
-- Parsing
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
