{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}


-- | Working with the Penn format.


module Odil.Penn
( Tree
, parseTree
, parseTree'
, parseForest
, parseFile
, dividePenn

-- * Showing
, showTree

-- * Conversion
, toOdilTree
, convertPennFile
) where


import Control.Monad (void, forM)
import Control.Applicative ((<|>))
import qualified Data.Functor.Identity as ID
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
        return $ Odil.Node i x Nothing

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


-- -- | Convert a list of pairs, each pair consisting of an (i) original sentence
-- -- and the (ii) corresponding tree, to an Odil file.
-- convertPennFile :: [(T.Text, Tree)] -> Odil.File
-- convertPennFile
--   = flip Odil.File S.empty
--   . M.fromList
--   -- . zip (map (T.pack . show) [1..])
--   . zip [1..]
--   . map (Arr.second toOdilTree)
--   -- . parseForest


-- | Convert a list of Penn trees to an Odil file.
convertPennFile :: [Tree] -> Odil.File
convertPennFile pennForest = ID.runIdentity $ do
  (turns, treeMap) <- flip ST.runStateT M.empty $ do
    forM pennForest $ \penn -> do
      let odil = toOdilTree penn
          sent = sentFromTree penn
      k <- ST.gets $ (+1) . M.size
      ST.modify' $ M.insert k (sent, odil)
      -- return (k, Nothing) -- speaker N/A
      return $ Odil.Turn
        { Odil.speaker = ["_"]      -- speakers N/A
        , Odil.trees = M.singleton k Nothing } -- speaker N/A
  return Odil.File
    { Odil.treeMap = treeMap
    , Odil.turns = turns -- concat turns
    , Odil.linkSet = M.empty
    }


-- | Make a raw sentence from a tree.
sentFromTree :: Tree -> T.Text
sentFromTree =
  runMkSent . getLeaves
  where
    getLeaves (R.Node x ts) = case ts of
      [] -> [x]
      _  -> concatMap getLeaves ts
    runMkSent = T.strip . T.intercalate " "
--     runMkSent = T.strip .  flip ST.evalState [] . mkSent
--     mkSent = \case
--       [] -> return ""
--       (x : xs) -> do
--         case x of
--           "," -> do
--             sent <- mkSent xs
--             return $ T.concat [x, " ", sent]
--           ":" -> do
--             sent <- mkSent xs
--             return $ T.concat [x, " ", sent]
--           ";" -> do
--             sent <- mkSent xs
--             return $ T.concat [x, " ", sent]
--           "\"" -> do
--             acc <- ST.get
--             case acc of
--               ("\"" : acc') -> do
--                 ST.put acc'
--                 sent <- mkSent xs
--                 if headPunc sent
--                   then return $ T.concat [x, sent]
--                   else return $ T.concat [x, " ", sent]
--               _ -> do
--                 ST.modify' ("\"":)
--                 sent <- mkSent xs
--                 return $ T.concat [" ", x, sent]
--           "(" -> do
--             sent <- mkSent xs
--             return $ T.concat [" ", x, sent]
--           ")" -> do
--             sent <- mkSent xs
--             if headPunc sent
--               then return $ T.concat [x, sent]
--               else return $ T.concat [x, " ", sent]
--           "%" -> do
--             sent <- mkSent xs
--             if headPunc sent
--               then return $ T.concat [x, sent]
--               else return $ T.concat [x, " ", sent]
--           _ -> do
--             sent <- mkSent xs
--             if (tailPunc x || headPunc sent)
--               then return $ T.concat [x, sent]
--               else return $ T.concat [x, " ", sent]
--     headPunc x = case T.findIndex C.isPunctuation x of
--       Just 0 -> True
--       _ -> False
--     tailPunc = headPunc . T.reverse
--     headIn xs bl = case xs of
--       hd:_ -> hd `elem` bl
--       _ -> True


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


parseTree' :: T.Text -> Maybe Tree
parseTree' x =
  case A.parseOnly (treeP <* A.endOfInput) x of
    Left err -> Nothing
    Right t -> Just t


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
leafP = flip R.Node [] . unEscape <$> labelP


---------------------------------------------------
---------------------------------------------------
-- Showing
---------------------------------------------------
---------------------------------------------------


showTree :: Tree -> T.Text
showTree (R.Node x ts)
  | null ts = escape x
  | otherwise = T.concat
    [ "(", x, " "
    , T.intercalate " " (map showTree ts)
    , ")"
    ]


---------------------------------------------------
-- Escaping
---------------------------------------------------


escape :: T.Text -> T.Text
escape x =
  case M.lookup x . M.fromList $ escapeList of
    Just y -> y
    Nothing -> x


unEscape :: T.Text -> T.Text
unEscape =
  L.foldl' (.) id replaceAll
  where
    replaceAll = map
      (uncurry T.replace)
      (map swap escapeList)
    swap (x, y) = (y, x)


-- | A list of escape characters.
escapeList :: [(T.Text, T.Text)]
escapeList =
  [ ("(", "-LRB-")
  , (")", "-RRB-")
  , ("[", "-RSB-")
  , ("]", "-RSB-")
  , ("{", "-LCB-")
  , ("}", "-RCB-")
  ]


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
