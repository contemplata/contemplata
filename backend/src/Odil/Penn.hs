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
, toOdilTree'
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

import Debug.Trace (trace)


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
        return $ Odil.Leaf i x p ""
      else do
        i <- newId
        return $ Odil.Node i x Nothing ""

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
  return $ Odil.mkNewFile treeMap turns


-- | Make a raw sentence from a tree.
sentFromTree :: Tree -> Odil.Sent
sentFromTree =
  getLeaves
  where
    getLeaves (R.Node x ts) = case ts of
      [] -> [token]
        where
          token = Odil.Token
            { Odil.orth = T.strip x
            , Odil.afterSpace = True }
      _  -> concatMap getLeaves ts


-- -- | Make a raw sentence from a tree.
-- sentFromTree :: Tree -> T.Text
-- sentFromTree =
--   runMkSent . getLeaves
--   where
--     getLeaves (R.Node x ts) = case ts of
--       [] -> [x]
--       _  -> concatMap getLeaves ts
--     runMkSent = T.strip . T.intercalate " "
-- --     runMkSent = T.strip .  flip ST.evalState [] . mkSent
-- --     mkSent = \case
-- --       [] -> return ""
-- --       (x : xs) -> do
-- --         case x of
-- --           "," -> do
-- --             sent <- mkSent xs
-- --             return $ T.concat [x, " ", sent]
-- --           ":" -> do
-- --             sent <- mkSent xs
-- --             return $ T.concat [x, " ", sent]
-- --           ";" -> do
-- --             sent <- mkSent xs
-- --             return $ T.concat [x, " ", sent]
-- --           "\"" -> do
-- --             acc <- ST.get
-- --             case acc of
-- --               ("\"" : acc') -> do
-- --                 ST.put acc'
-- --                 sent <- mkSent xs
-- --                 if headPunc sent
-- --                   then return $ T.concat [x, sent]
-- --                   else return $ T.concat [x, " ", sent]
-- --               _ -> do
-- --                 ST.modify' ("\"":)
-- --                 sent <- mkSent xs
-- --                 return $ T.concat [" ", x, sent]
-- --           "(" -> do
-- --             sent <- mkSent xs
-- --             return $ T.concat [" ", x, sent]
-- --           ")" -> do
-- --             sent <- mkSent xs
-- --             if headPunc sent
-- --               then return $ T.concat [x, sent]
-- --               else return $ T.concat [x, " ", sent]
-- --           "%" -> do
-- --             sent <- mkSent xs
-- --             if headPunc sent
-- --               then return $ T.concat [x, sent]
-- --               else return $ T.concat [x, " ", sent]
-- --           _ -> do
-- --             sent <- mkSent xs
-- --             if (tailPunc x || headPunc sent)
-- --               then return $ T.concat [x, sent]
-- --               else return $ T.concat [x, " ", sent]
-- --     headPunc x = case T.findIndex C.isPunctuation x of
-- --       Just 0 -> True
-- --       _ -> False
-- --     tailPunc = headPunc . T.reverse
-- --     headIn xs bl = case xs of
-- --       hd:_ -> hd `elem` bl
-- --       _ -> True


---------------------------------------------------
-- Conversion with syncronization
---------------------------------------------------


type SyncElem = (Odil.Token, Maybe T.Text)


type SyncStack = [SyncElem]


-- | Syncronization state.
data Sync = Sync
  { left :: SyncStack
  , right :: SyncStack
  }


-- | Conver a given Penn tree to an ODIL tree.
toOdilTree'
  :: Tree
     -- ^ The Penn tree
  -> SyncStack
     -- ^ The list of original tokens and the corresponding, pre-processed
     -- tokens. The latter align with the given Penn tree.
  -> (Odil.Sent, Odil.Tree)
toOdilTree' tree0 toks0

  = finalize
  . flip ST.runState (Sync [] toks0)
  . Trav.traverse f
  . markLeaves
  . trace (show tree0)
  $ tree0

  where

    finalize (tree, sync) =
      ( reverse . map fst $ left sync
      , tree )

    markLeaves (R.Node x ts) = case ts of
      [] -> R.Node (x, True) []
      _  -> R.Node (x, False) (map markLeaves ts)

    f (x, isLeaf) =
      if isLeaf then do
        processLeaf x
        p <- (\k->k-1) . length . left <$> ST.get
        return $ Odil.Leaf 0 x p ""
      else do
        return $ Odil.Node 0 x Nothing ""

    -- | Process the leaf of the Penn tree.
    processLeaf :: T.Text -> ST.State Sync ()
    processLeaf leafTxt = do
      _ <- ST.gets left >>= \x -> trace ("left: " ++ show x) (return x)
      _ <- ST.gets right >>= \x -> trace ("right: " ++ show x) (return x)

      mayHead <- popRight
      case mayHead of
        Nothing -> do
          error "Penn.toOdilTree'.processLeaf: empty right"
        Just (tok, mayTxt) -> case mayTxt of
          Nothing -> pushLeft (tok, mayTxt) >> processLeaf leafTxt
          Just tokTxt ->
            if leafTxt == tokTxt then do
              pushLeft (tok, mayTxt)
            else if leafTxt `T.isPrefixOf` tokTxt then do
              let restTxt = stripPrefix leafTxt tokTxt
              let (tokLeft, tokRight) = splitTok leafTxt tok
              pushLeft (tokLeft, Just leafTxt)
              pushRight (tokRight, Just restTxt)
            else
              error "Penn.toOdilTree'.processLeaf: don't handle this branch yet"

--     -- | Shift the top `right` stack element to the `left` stack`.
--     shift :: ST.State Sync ()
--     shift = popRight >>= pushLeft

    popRight :: ST.State Sync (Maybe SyncElem)
    popRight = do
      sync <- ST.get
      case right sync of
        hd : tl -> do
          ST.put $ sync {right = tl}
          return $ Just hd
        [] -> return Nothing

    pushLeft :: SyncElem -> ST.State Sync ()
    pushLeft x = do
      sync <- ST.get
      ST.put $ sync {left = x : left sync}

    pushRight :: SyncElem -> ST.State Sync ()
    pushRight x = do
      sync <- ST.get
      ST.put $ sync {right = x : right sync}

--     newId = do
--       conv <- ST.get
--       ST.put $ conv {idAcc = idAcc conv + 1}
--       return $ idAcc conv
--
--     newPos = do
--       conv <- ST.get
--       ST.put $ conv {posAcc = posAcc conv + 1}
--       return $ posAcc conv


-- | Split the given (longer) token w.r.t. the given (shorter) leaf text.
splitTok
  :: T.Text
     -- ^ The shorter text
  -> Odil.Token
     -- ^ The token to split and the corresponding text
  -> (Odil.Token, Odil.Token)
splitTok =

  \x -> finalize . go x

  where

    finalize (left, right) =
      let left' = stripTok left
          right' = stripTok right
      in  (left', right')

--     finalize (left, right) =
--       let left' = stripTok left
--           right' =
--             let tok = stripTok right
--             in  if lastSpace left
--                 then tok {Odil.afterSpace = True}
--                 else tok
--       in  (left', right')
--
--     lastSpace tok = " " `T.isSuffixOf` Odil.orth tok

    go pref tok
      | T.null pref =
          let emptyTok = Odil.Token "" False
          in  (emptyTok, tok)
      | T.head pref == tokHead =
          let (left, right) = go (T.tail pref) (orthTail tok)
          in  (orthCons tokHead left, right)
      | C.isSpace tokHead =
          let (left, right) = go pref (orthTail tok)
          in  (orthCons tokHead left, right)
      | otherwise = error "splitTok: not implemented yet"
      where
        tokHead = T.head (Odil.orth tok)


-- | Apply `T.tail` on the token's orth.
orthTail :: Odil.Token -> Odil.Token
orthTail tok = tok { Odil.orth = T.tail (Odil.orth tok) }


-- | Apply `T.cons` on the token's orth.
orthCons :: Char -> Odil.Token -> Odil.Token
orthCons c tok = tok { Odil.orth = T.cons c (Odil.orth tok) }


-- | Strip the token from spaces and update info about `afterSpace`.
stripTok :: Odil.Token -> Odil.Token
stripTok tok = tok
  { Odil.orth = T.strip (Odil.orth tok)
  , Odil.afterSpace =
      if " " `T.isSuffixOf` Odil.orth tok
      then True
      else False
  }


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


stripPrefix :: T.Text -> T.Text -> T.Text
stripPrefix pref txt =
  case T.stripPrefix pref txt of
    Nothing -> error "Penn.stripPrefix: not a prefix!"
    Just x -> x
