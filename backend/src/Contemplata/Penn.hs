{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}


-- | Working with the Penn format.


module Contemplata.Penn
( Tree
, parseTree
, parseTree'
, parseForest
, parseFile
, dividePenn

-- * Showing
, showTree

-- * Conversion
, toContemplataTree
, toContemplataTree'
, toContemplataForest
, convertPennFile
) where


import Control.Monad (void, forM)
import Control.Applicative ((<|>))
import qualified Data.Functor.Identity as ID
import qualified Control.Arrow as Arr
import qualified Control.Monad.Trans.State.Strict as ST

import qualified Data.Traversable as Trav
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


import qualified Contemplata.Server.Types as Contemplata


---------------------------------------------------
-- Types
---------------------------------------------------


-- | A Penn tree.
type Tree = R.Tree T.Text


---------------------------------------------------
---------------------------------------------------
-- Conversion
---------------------------------------------------
---------------------------------------------------


-- | Conversion state.
data Conv = Conv
  { idAcc :: Int
  , posAcc :: Int }


-- | Conver a given Penn tree to an ODIL tree.
toContemplataTree :: Tree -> Contemplata.Tree
toContemplataTree =

  flip ST.evalState
    (Conv 0 0)
  . Trav.traverse f
  . markLeaves

  where

    f (x, isLeaf) =
      if isLeaf then do
        i <- newId
        p <- newPos
        return $ Contemplata.Leaf i x p ""
      else do
        i <- newId
        return $ Contemplata.Node i x Nothing ""

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


-- | Convert a list of Penn trees to an Contemplata file.
convertPennFile :: [Tree] -> Contemplata.File
convertPennFile pennForest = ID.runIdentity $ do
  (turns, treeMap) <- flip ST.runStateT M.empty $ do
    forM pennForest $ \penn -> do
      let odil = toContemplataTree penn
          sent = sentFromTree penn
      k <- ST.gets $ (+1) . M.size
      ST.modify' $ M.insert k (sent, odil)
      -- return (k, Nothing) -- speaker N/A
      return $ Contemplata.Turn
        { Contemplata.speaker = ["_"]      -- speakers N/A
        , Contemplata.trees = M.singleton k Nothing } -- speaker N/A
  return $ Contemplata.mkNewFile treeMap turns


-- | Make a raw sentence from a tree.
sentFromTree :: Tree -> Contemplata.Sent
sentFromTree =
  getLeaves
  where
    getLeaves (R.Node x ts) = case ts of
      [] -> [token]
        where
          token = Contemplata.Token
            { Contemplata.orth = T.strip x
            , Contemplata.afterSpace = True }
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


type SyncElem = (Contemplata.Token, Maybe T.Text)


type SyncStack = [SyncElem]


-- | Syncronization state.
data Sync = Sync
  { left :: SyncStack
  , right :: SyncStack
  }



-- | Convert a given Penn tree to an ODIL tree.
--
-- WARNING: the trees in the result are not guaranteed to have different node
-- IDs.  This is because some subtrees are unknown (`Nothing`) and there is no
-- way to take their node IDs into account anyway.
toContemplataForest
  :: [Maybe Tree]
     -- ^ The Penn (maybe) forest
  -> [SyncStack]
     -- ^ The list of original tokens and the corresponding, pre-processed
     -- tokens. The latter align with the corresponding Penn tree.
  -> [Maybe Contemplata.Tree]
toContemplataForest =
  go 0
  where
    go acc forest tokList =
      case (forest, tokList) of
        (Nothing : forestRest, toks : tokRest) ->
          Nothing : go (acc + length toks) forestRest tokRest
        (Just tree : forestRest, toks : tokRest) ->
          Just (shift acc newTree) :
          -- go (acc + length newToks) forestRest tokRest
          go (acc + length toks) forestRest tokRest
          where (_newToks, newTree) = toContemplataTree' tree toks
        _ -> []
    shift k = fmap $ \node -> case node of
      Contemplata.Node{} -> node
      Contemplata.Leaf{..} -> node {Contemplata.leafPos = leafPos + k}


-- | Convert a given Penn tree to an ODIL tree.
toContemplataTree'
  :: Tree
     -- ^ The Penn tree
  -> SyncStack
     -- ^ The list of original tokens and the corresponding, pre-processed
     -- words (some of which can be removed, indicated by `Nothing`).
  -> (Contemplata.Sent, Contemplata.Tree)
     -- ^ The result is a pair:
     --   (i) an ODIL sentence, i.e., a list of tokens which should corresond to
     --     a prefix of the input `SyncStack` (more precisely, the original
     --     `Token`s)
     --   (ii) an ODIL tree, whose leaves align with the `Just` words in the
     --     input `SyntStack`.
toContemplataTree' tree0 toks0

  = finalize
  . flip ST.runState (Sync [] toks0)
  . Trav.traverse f
  . markLeaves
  . trace (show tree0)
  $ tree0

  where

    finalize (tree, sync) =
      ( reverse . map fst $ left sync
      , reID tree )

    markLeaves (R.Node x ts) = case ts of
      [] -> R.Node (x, True) []
      _  -> R.Node (x, False) (map markLeaves ts)

    f (x, isLeaf) =
      if isLeaf then do
        processLeaf x
        p <- (\k->k-1) . length . left <$> ST.get
        return $ Contemplata.Leaf 0 x p ""
      else do
        return $ Contemplata.Node 0 x Nothing ""

    -- | Process the leaf of the Penn tree.
    processLeaf :: T.Text -> ST.State Sync ()
    processLeaf leafTxt = do
      _ <- ST.gets left >>= \x -> trace ("left: " ++ show x) (return x)
      _ <- ST.gets right >>= \x -> trace ("right: " ++ show x) (return x)

      mayHead <- popRight
      case mayHead of
        Nothing -> do
          error "Penn.toContemplataTree'.processLeaf: empty right"
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
              error "Penn.toContemplataTree'.processLeaf: don't handle this branch yet"

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
  -> Contemplata.Token
     -- ^ The token to split and the corresponding text
  -> (Contemplata.Token, Contemplata.Token)
splitTok pref0 tok0 =

  finalize $ go pref0 tok0

  where

    finalize (left, right) =
      let left' = stripTok left
            {Contemplata.afterSpace = Contemplata.afterSpace tok0}
          right' = stripTok right
            {Contemplata.afterSpace = " " `T.isPrefixOf` Contemplata.orth right}
      in  (left', right')

    go pref tok
      | T.null pref =
          let emptyTok = Contemplata.Token "" True
              -- JW (9/11): afterSpace = False or True,
              -- it should make no difference
          in  (emptyTok, tok)
      | T.head pref == tokHead =
          let (left, right) = go (T.tail pref) (orthTail tok)
          in  (orthCons tokHead left, right)
      | C.isSpace tokHead =
          let (left, right) = go pref (orthTail tok)
          in  (orthCons tokHead left, right)
      | otherwise = error "splitTok: not implemented yet"
      where
        tokHead = T.head (Contemplata.orth tok)


-- | Apply `T.tail` on the token's orth.
orthTail :: Contemplata.Token -> Contemplata.Token
orthTail tok = tok { Contemplata.orth = T.tail (Contemplata.orth tok) }


-- | Apply `T.cons` on the token's orth.
orthCons :: Char -> Contemplata.Token -> Contemplata.Token
orthCons c tok = tok { Contemplata.orth = T.cons c (Contemplata.orth tok) }


-- | Strip the token from spaces and update info about `afterSpace`.
stripTok :: Contemplata.Token -> Contemplata.Token
stripTok tok = tok
  { Contemplata.orth = T.strip (Contemplata.orth tok)
--   , Contemplata.afterSpace =
--       -- JW (9/11): isSuffixOf -> isPrefixOf
--       if " " `T.isPrefixOf` Contemplata.orth tok
--       then True
--       else False
  }


-- | Re-identify the nodes in the given tree.
-- The old identifiers are discarded.
reID :: Contemplata.Tree -> Contemplata.Tree
reID =
  snd . Trav.mapAccumL update 0
  where
    update k node = (k + 1, setId k node)
    setId k node = case node of
      Contemplata.Leaf{} -> node {Contemplata.leafId = k}
      Contemplata.Node{} -> node {Contemplata.nodeId = k}


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
