{-# LANGUAGE ParallelListComp, ViewPatterns #-}
import qualified Data.ByteString as B
import qualified Data.List as L
import qualified Data.Map as M

import qualified Data.Set as S
import qualified Data.Text.Encoding as E
import qualified Data.Text as T

import Data.Char ( isAlphaNum )
import Data.Maybe ( catMaybes )
import Data.Ord ( comparing )
import Data.Tuple ( swap )

import Control.Parallel.Strategies
import Control.DeepSeq ( NFData, force, rnf )

mrlist :: NFData b => (a -> b) -> [a] -> [b]
mrlist m xs =
  runEval $ do
    mapM (rpar . force . m) xs

mr :: NFData b => (b -> b -> b) -> (a -> b) -> [a] -> b
mr f m xs =
  runEval $ do
    ms <- mapM (rpar . force . m) xs
    return $ xfoldl1 f ms

xfoldl1 f [x] = x
xfoldl1 f (a:b:xs) = xfoldl1 f $ redu (a:b:xs)
  where redu (a:b:xs) = f a b : redu xs
        redu xs       = xs


type TextChunks = [T.Text]

bytestringChunksOf :: Int -> B.ByteString -> [B.ByteString]
bytestringChunksOf len bs 
  | B.length bs <= len = [bs]
  | otherwise =
    let (a,b) = B.splitAt len bs
    in a : bytestringChunksOf len b
  

readFileIntoChunks filename =
  do c <- B.readFile filename
     let cs = bytestringChunksOf (1024*1024) c
     return $ mrlist E.decodeUtf8 cs

shortenChunks sz chunks = map (T.take sz) chunks


reportNumberOfSpacesAndNewlines chunks =
  do let numSpaces = mr (+) (T.count (T.pack " ")) chunks
         numNewlines = mr (+) (T.count (T.pack "\n")) chunks
         numQuotes = mr (+) (T.count (T.pack "'")) chunks
     putStrLn ("number of spaces: " ++ show numSpaces
               ++ ", newlines: " ++ show numNewlines
               ++ "  and quotes: " ++ show numQuotes)
     
-- tokenize: find all transitions from word chars to non-word chars
--   only group word-chars, all non-word chars are separate tokens

wordsOfChunk chunk =
  [ word
  | word0 <- T.words chunk
  , word <- nwc word0 0 ]
  where
    nwc c pos | pos == T.length c = []
    nwc c pos =
      let ch = T.index c pos
      in if isAlphaNum ch 
         then wc c pos (succ pos)
         else T.singleton ch : nwc c (succ pos)
    wc c start pos | pos == T.length c = if pos > start then [subtext c start pos] else []
    wc c start pos =
      let ch = T.index c pos
      in if isAlphaNum ch 
         then wc c start (succ pos)
         else subtext c start pos : nwc c pos
    subtext c start end = T.take (end-start) (T.drop start c)


-- find all common words and all common ends

commonWords :: TextChunks -> M.Map T.Text Int
commonWords chunks = mr (M.unionWith (+)) ex chunks
  where ex chunk = M.fromListWith (+) 
                   [ (word, 1) | word <- wordsOfChunk chunk ]
        
commonEnds :: TextChunks -> M.Map T.Text Int
commonEnds chunks = mr (M.unionWith (+)) ex chunks
  where ex chunk = M.fromListWith (+) 
                   [ (suffix, 1) 
                   | word <- wordsOfChunk chunk 
                   , suffix <- allWordEnds word ]
                   
-- all ends of a word, most specific first (i.e. the longest)
allWordEnds :: T.Text -> [T.Text]
allWordEnds = reverse . take 5 . tail . reverse . T.tails

someCommonWords :: Int -> TextChunks -> [T.Text]
someCommonWords n chunks = largestMapValues n $ commonWords chunks

someCommonEnds :: Int -> TextChunks -> [T.Text]
someCommonEnds n chunks = largestMapValues n $ commonEnds chunks

largestMapValues :: (Ord v) => Int -> M.Map k v -> [k]
largestMapValues n theMap = map fst $ largestMapValuesWithKeys n theMap

largestMapValuesWithKeys :: (Ord v) => Int -> M.Map k v -> [(k,v)]
largestMapValuesWithKeys n theMap =
  take n $
  reverse $
  L.sortBy (comparing snd) $ 
  M.toList $
  theMap

data Context = TwoLeftTwoRight T.Text T.Text T.Text T.Text
             deriving (Eq, Ord, Show)
instance NFData Context where                      
  rnf (TwoLeftTwoRight x1 x2 x3 x4) = 
    rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4           

-- | 'wordsInContext' extracts all words with some bits of context
wordsInContext :: [T.Text] -> [T.Text] -> TextChunks -> M.Map Context (M.Map T.Text Int)
wordsInContext commonWords commonEnds chunks = 
    mr (M.unionWith (M.unionWith (+))) ex chunks
  where ex chunk = 
          let ws = wordsOfChunk chunk
              ends = map wordExtract ws :: [T.Text]
          in M.fromList
             [ (ctx, M.singleton word 1)
             | word <- tail $ tail $ ws
             | (l2:l1:_:r1:r2:_) <- L.tails ends
             , let ctx = TwoLeftTwoRight l2 l1 r1 r2 ]
        wordExtract :: T.Text -> T.Text
        wordExtract word | S.member word commonWordsSet = word
        wordExtract word = 
          head $
          (catMaybes $
           map (\e -> if S.member e commonEndsSet then Just e else Nothing) $
           allWordEnds word) 
          ++ [T.empty]
        commonWordsSet = S.fromList commonWords
        commonEndsSet = S.fromList commonEnds


printWordsInContext :: Int -> M.Map Context (M.Map T.Text Int) -> IO ()
printWordsInContext n mapmap =
  sequence_
  [ do print sum
       printContext ctx
       -- putStrLn $ T.unpack word
       -- mapM_ printContext (L.sortBy (comparing snd) $ M.toList ctxs)
       mapM_ print $
         reverse $
         L.sortBy (comparing snd) $
         M.toList wordsWithCounts
  | (sum, (ctx, wordsWithCounts)) <-
       take n $
       reverse $
       L.sortBy (comparing fst) $
       map (\(k,v) -> (sum $ map snd $ M.toList v, (k,v))) $
       M.toList mapmap ]
  where printContext (TwoLeftTwoRight l2 l1 r1 r2) =
          do putStrLn $ L.intercalate "=" $ map (T.unpack) [l2, l1, r1, r2]

wordsConnectedByContext :: M.Map Context (M.Map T.Text Int) -> M.Map (T.Text, T.Text) Int
wordsConnectedByContext mapmap =
  M.unionsWith (+)
  [ M.fromList [ ((aword,bword), mcount), ((bword,aword), mcount) ]
  | (context, wordsWithCounts) <- M.toList mapmap
  , (aword, acount) <- M.toList wordsWithCounts
  , (bword, bcount) <- M.toList wordsWithCounts
  , aword /= bword
  , let mcount = min acount bcount ]


{-
chineseWhisper :: M.Map (T.Text, T.Text) Int -> IO ()
chineseWhisper connectedWords =
  do let stree = undefined
     return ()
  where connections =
          M.unionsWith (M.unionWith (+))
          [ M.singleton worda (M.singleton wordb count)
          | ((worda, wordb), count) <- M.toList connectedWords ]
          :: M.Map T.Text (M.Map T.Text Int)
        probs = M.map buildProbs connections
        buildProbs wordcountmap =
          ...
          L.sortBy (comparing fst) $
          map swap $ M.toList wordcountmap
          ...
        allWords = map fst $ M.toList connections
-}
           
           
main =     
  do t1 <- readFileIntoChunks "input.txt"
     let t = if True then t1 else shortenChunks 3000 t1
     reportNumberOfSpacesAndNewlines t
     let ws = someCommonWords 200 t
     let cs = someCommonEnds 200 t
     let vs = wordsInContext ws cs t
     
     printWordsInContext 100 vs
     
     let connectedWords = wordsConnectedByContext vs
     putStrLn "\nwords connected by context:"
     mapM_ print $ largestMapValuesWithKeys 200000 connectedWords

     -- chineseWhisper connectedWords