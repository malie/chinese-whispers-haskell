{-# LANGUAGE ParallelListComp, ViewPatterns #-}
import qualified Data.ByteString as B
import qualified Data.List as L
import qualified Data.Map as M

import qualified Data.Set as S
import qualified Data.Text.Encoding as E
import qualified Data.Text as T

import Data.Char ( isAlphaNum , isDigit , isPunctuation )
import Data.Maybe ( catMaybes, isNothing, fromJust )
import Data.Ord ( comparing )
import Data.Tuple ( swap )

import Control.Monad ( liftM )
import Control.Parallel.Strategies
import Control.DeepSeq ( NFData, force, rnf )

import qualified System.Random as R
import Debug.Trace ( trace )

import Cwlib (buildNeighbors)

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
     -- return $ mrlist E.decodeUtf8 cs
     return $ mrlist E.decodeLatin1 cs

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
      in if isAlphaNum ch || ch == '\''
         then wc c pos (succ pos)
         else T.singleton ch : nwc c (succ pos)
    wc c start pos | pos == T.length c =
      if pos > start then [subtext c start pos] else []
    wc c start pos =
      let ch = T.index c pos
      in if isAlphaNum ch || ch == '-'
         then wc c start (succ pos)
         else subtext c start pos : nwc c pos
    subtext c start end =
      T.pack $ generalize $ T.unpack $
      T.take (end-start) (T.drop start c)
    generalize word =
      let recur (c:cs) res
            | isDigit c = nines cs (c:res)
            | otherwise = recur cs (c:res)
          recur [] res = reverse res
          nines (c:cs) res | isDigit c = nines cs ('9':res)
          nines cs res  = recur cs res
      in recur word []
    

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
             | OneLeftOneRight T.Text Int Int T.Text
             | CoOccurs T.Text Int
             deriving (Eq, Ord, Show)
                      
instance NFData Context where                      
  rnf (TwoLeftTwoRight x1 x2 x3 x4) = 
    rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4          
  rnf (OneLeftOneRight x1 x2 x3 x4) = 
    rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4          
  rnf (CoOccurs x1 x2) = 
    rnf x1 `seq` rnf x2

-- | 'wordsInContext' extracts all words with some bits of context
wordsInContext :: S.Set T.Text -> [T.Text] -> [T.Text] -> TextChunks
                  -> M.Map Context (M.Map T.Text Int)
wordsInContext interestingWords commonWords commonEnds chunks = 
    mr (M.unionWith (M.unionWith (+))) ex chunks
  where ex chunk = 
          let ws = wordsOfChunk chunk
              ends = map wordExtract ws :: [T.Text]
          in M.fromListWith (M.unionWith (+))
             [ (ctx, M.singleton word 1)
             | (word, (l2:l1:_:r1:r2:_))
                  <- zip (tail $ tail $ ws) (L.tails ends)
             , S.member word interestingWords
             , let ctx = TwoLeftTwoRight l2 l1 r1 r2 ]
        wordExtract :: T.Text -> T.Text
        wordExtract word | S.member word commonWordsSet = word
        wordExtract word = 
          head $
          (catMaybes $
           map (\e -> if S.member e commonEndsSet
                      then Just e
                      else Nothing) $
           allWordEnds word) 
          ++ [T.empty]
        commonWordsSet = S.fromList commonWords
        commonEndsSet = S.fromList commonEnds

-- 'w' is the target word, 'x' and 'y' are context:
-- x w y
-- x w _ y
-- x w _ _ y
-- x _ w y
-- x _ w _ y
-- x _ w _ _ y
wordsInContext2 :: Int -> TextChunks -> M.Map Context (M.Map T.Text Int)
wordsInContext2 d chunks = mr (M.unionWith (M.unionWith (+))) ex chunks
  where ex chunk =
          M.fromListWith (M.unionWith (+))
          [ (OneLeftOneRight x ignLeft ignRight y, M.singleton w 1)
          | (x:xs) <- L.tails $ wordsOfChunk chunk
          , ignLeft <- [0..(d-1)]
          , ignRight <- [0..(d-1)]
          , (w:ws) <- [drop ignLeft xs]
          , (y:_) <- [drop ignRight ws] ]

wordsInContext3 :: Int -> S.Set T.Text -> S.Set T.Text -> TextChunks 
                   -> M.Map Context (M.Map T.Text Int)
wordsInContext3 maxdist featureWords interestingWords chunks =
  mr (M.unionWith (M.unionWith (+))) ex chunks
  where ex chunk =
          M.fromListWith (M.unionWith (+)) $
          concat $
          [ (if S.member w interestingWords
                && S.member x featureWords
             then [ (CoOccurs x (-ign-1), M.singleton w 1)]
             else [])
            ++ (if S.member x interestingWords
                   && S.member w featureWords
                then [ (CoOccurs w (ign+1), M.singleton x 1) ]
                else [])
          | (x:xs) <- L.tails $ wordsOfChunk chunk
          , ign <- [0..(maxdist-1)]
          , not $ any isPunctuationWord $ take ign xs
          , (w:_) <- [drop ign xs]]

isPunctuationWord w = T.length w == 1
                      && isPunctuation (T.head w)

withCountGT lim mapmap =
  M.fromList
  [ (context, M.fromList [ (word, count)
                         | (word, count) <- M.toList mp
                         , count > lim ])
  | (context, mp) <- M.toList mapmap ]


printWordsInContext :: Int -> M.Map Context (M.Map T.Text Int) -> IO ()
printWordsInContext n mapmap =
  sequence_
  [ do print ("word in context:", contextToString ctx, "len:", sum)
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
  where contextToString (TwoLeftTwoRight l2 l1 r1 r2) =
          L.intercalate "=" $ map (T.unpack) [l2, l1, r1, r2]
        contextToString (OneLeftOneRight l el er r) =
          L.intercalate "=" 
          [T.unpack l, show el, show er, T.unpack r]
        contextToString (CoOccurs x d) =
          T.unpack x ++ show d

contextWeight (TwoLeftTwoRight _ _ _ _) = 10
contextWeight (OneLeftOneRight _ _ _ _) = 1
contextWeight (CoOccurs _ x) = 
  case abs x of
    0 -> 100
    1 -> 10
    2 -> 3
    _ -> 1
    


wordsConnectedByContext :: M.Map Context (M.Map T.Text Int)
                           -> M.Map (T.Text, T.Text) Int
wordsConnectedByContext mapmap =
  M.unionsWith (+)
  [ M.fromList $ 
    -- take 200 $
    reverse $
    L.sortBy (comparing snd) $ 
    M.toList $
    M.unionsWith (+)
    [ M.fromList [ ((aword,bword), mcount), ((bword,aword), mcount) ]
    | (aword, acount) <- wwc
    , (bword, bcount) <- wwc
    -- , acount > 20 && bcount > 10
    , acount > bcount -- aword < bword
    , acount <= 2*bcount
    , bcount > 1
    , let mcount =
            -- round (1000 * sqrt (fromIntegral (acount * bcount))) -- +
            -- square (fromIntegral $ 10000 * (acount + bcount) `div` su) -- +
            -- (fromIntegral $ 10000 * (acount + bcount) `div` su)
            -- round ((fromIntegral $ 1000 * (acount + bcount) `div` su) ** 0.5)
            -- round ((fromIntegral $ 1000 * (acount + bcount) `div` su) ** 2)
            -- 1 + round (log (fromIntegral $ (acount + bcount) `div` su) * 3) -- +
            -- min acount bcount
            -- max acount bcount
            -- acount * bcount
            -- acount + bcount
            -- 1000 * (acount * bcount) `div` (acount + bcount)
            1
            * contextWeight context
    ]
  | (context, wordsWithCounts) <- M.toList mapmap
  , let wwc = take 50 $ reverse $ L.sortBy (comparing snd) $ 
              M.toList wordsWithCounts
        su = sum $ map snd wwc
  ]

-- maybe absolute probabilities of the individual words are useful here


square x = x*x


randomListElement :: [a] -> IO a
randomListElement [] = error "empty list"
randomListElement list = 
  do i <- R.getStdRandom (R.randomR (0, length list-1))
     return $ list !! i

-- input is ordered, so max values are at start, choose some random maximum
chooseSomeMaximumAtStart :: (Ord a) => [(T.Text, a)] -> IO T.Text
chooseSomeMaximumAtStart ((t,a):(_,b):_) | b < a = return t
chooseSomeMaximumAtStart ((t,a):xs) =
  do let options = t : [ x | (x,v) <- xs, v == a ]
         len = length options
     randomListElement options
chooseSomeMaximumAtStart [] = return $ T.pack "?"

-- input graph to chinese whispers, weigthed
type Graph a = M.Map T.Text [(T.Text, a)]

-- assignment of word class to each word, primary chineseWhispers state
type Assignment = M.Map T.Text T.Text


chineseWhispers :: (Ord a, Num a) => Int -> Graph a -> IO Assignment
chineseWhispers maxIterations graph = iterate 0 initial 
    where 
      initial :: Assignment
      initial = M.fromList [(k, k) | (k, _) <- M.toList graph]
          
      -- majorClass :: Maybe [(T.Text, Int)] -> Assignment -> IO T.Text
      majorClass Nothing m = return $ T.pack "?"
      majorClass (Just nl) m = 
        chooseSomeMaximumAtStart $
        reverse $
        L.sortBy (comparing snd) $ 
        M.toList $ M.fromListWith (+) 
        [(nclas, nc) 
        | (nw, nc) <- nl
        , Just nclas <- [M.lookup nw m]]
      iterate :: Int -> Assignment -> IO Assignment
      iterate n s | n == maxIterations = return s
      iterate n s = 
        do putStrLn $ "ITERATIONS " ++ show n
           printAssignments 29 graph s
           xs <- mapM (\(k, oc) ->
                        do newclass <-
                             chooseRandomly 0.9
                             (majorClass (M.lookup k graph) s)
                               (return oc)
                           return (k, newclass))                       
                 (M.toList s)
           iterate (succ n) $ M.fromList xs

chooseRandomly :: Float -> IO a -> IO a -> IO a
chooseRandomly probability a b =
  do i <- R.getStdRandom (R.randomR (0.0, 1.0))
     if i < probability
       then a
       else b

someRandomListElements 0 _ = return []
someRandomListElements _ [] = return []
someRandomListElements n xs =
  do el <- randomListElement xs
     rest <- someRandomListElements (pred n) (L.delete el xs)
     return $ el : rest

printAssignments :: (Ord a, Num a) => Int -> Graph a -> Assignment -> IO ()
printAssignments n g s =
  do putStrLn "\nstats"
     let clusters = M.unionsWith (++) [ M.singleton c [w] | (w,c) <- M.toList s ] 
         largestClusters = take n $ map snd $ reverse $ L.sortBy (comparing fst)
                           [ (length words, x)
                           | x@(_, words) <- M.toList clusters ]
     sequence_
       [ do let cwords = orderWordsByClosenessTo g c words
            putStrLn $ T.unpack c ++ " -- " ++ show (length words) ++ " words"
            putStrLn $ joinWords "    " $ take 100 cwords
       | (c, words) <- largestClusters ]

joinWords prefix words =
  let (xs,ys) = L.splitAt 7 words
  in prefix 
     ++ L.intercalate " " (map T.unpack xs)
     ++ if ys /= []
        then "\n" ++ joinWords prefix ys
        else ""

splitAtHeadAndTail :: Int -> [a] -> ([a], [a], [a])
splitAtHeadAndTail n list =
  let (start, rest1) = L.splitAt n list
      (end, rest) = L.splitAt n $ reverse rest1
  in ( start
     , reverse rest
     , reverse end)

orderWordsByClosenessTo :: (Ord a, Num a) => 
                           Graph a -> T.Text -> [T.Text] -> [T.Text]
orderWordsByClosenessTo g centerWord clusterWords = 
    map fst $ rec 30 [(centerWord, 1000000)]
  where rec 0 ws = ws
        rec n ws = reverse $
                   L.sortBy (comparing snd) $
                   M.toList $ M.fromListWith (+) $
                   concat
                     [ [(word, weight)]
                       ++ [ (neighbour, nweight)
                          | Just edges <- [M.lookup word g]
                          , (neighbour, nweight) <- edges
                          , S.member neighbour clusterWordsSet ]
                     | (word, weight) <- ws ]
        clusterWordsSet = S.fromList clusterWords

joinCommonWords :: [T.Text] -> TextChunks -> TextChunks
joinCommonWords commonWords chunks = mr (++) join chunks
  where cs = S.fromList commonWords
        join chunk = [T.intercalate (T.pack " ") $ rec ( wordsOfChunk chunk)]
        rec (a:b:xs) | S.member a cs && S.member b cs = 
          T.append (T.append a (T.pack "zz")) b : rec xs
        rec (a:xs) = a : rec xs
        rec []     = []



findToLargeClusters :: Int -> Assignment -> [T.Text]
findToLargeClusters maxSize assignment =
  -- take 2 $ map fst $ reverse $ L.sortBy (comparing snd) $
  map fst $ filter (\ (cl, size) -> size > maxSize) $
  M.toList $
  M.fromListWith (+)
  [ (word, 1)
  | word <- map snd $ M.toList assignment ]
  

dropSomeWords :: Float -> [T.Text]
                 -> M.Map Context (M.Map T.Text Int)
                 -> IO (M.Map Context (M.Map T.Text Int))
dropSomeWords rate largeClusters connectedWords =
  liftM M.fromList $ mapM dcontext $ M.toList connectedWords
  where dcontext (ctx, words) =
          do ws <- mapM dwords $ M.toList words
             return (ctx, M.fromList $ catMaybes ws)
        lcSet = S.fromList largeClusters
        dwords (word, count) 
          | S.notMember word lcSet = 
            return $ Just (word, count)
          | otherwise =
            chooseRandomly rate
              (return Nothing)
              (return $ Just (word, count))

dropSomeWordsX :: Float -> M.Map Context (M.Map T.Text Int)
                 -> IO (M.Map Context (M.Map T.Text Int))
dropSomeWordsX rate connectedWords =
  liftM M.fromList $ mapM dcontext $ M.toList connectedWords
  where dcontext (ctx, words) =
          do ws <- mapM dwords $ M.toList words
             return (ctx, M.fromList $ catMaybes ws)
        dwords (word, count) 
          | otherwise =
            chooseRandomly rate
              (return Nothing)
              (return $ Just (word, count))


buildLexicalClusters =     
  do t1 <- readFileIntoChunks "input.txt"
     putStrLn $ "number of text chunks:" ++ show (length t1)
     let t = if False then t1 else shortenChunks 100000 t1
     reportNumberOfSpacesAndNewlines t
     let cs = someCommonEnds 300 t
     let commonWords = someCommonWords 10000 t
     let veryCommonWords = take 200 commonWords
     let ws = take 400 commonWords
     let interestingWords =
           S.difference
           (S.fromList commonWords)
           (S.fromList veryCommonWords)
           
     
     let numIterations = 50
         shrinkThenClusterAgain n vs1 assignment
           | n == numIterations = return assignment
           | otherwise =
             do let largeClusters = findToLargeClusters 500 assignment
                print ("large clusters:", largeClusters)
                vs2 <- dropSomeWords 0.4 largeClusters vs1
                let connectedWords = wordsConnectedByContext vs2
                putStrLn $ "\n\nshrink iteration " ++ show n
                res <- chineseWhispers 20 $
                       buildNeighbors connectedWords
                shrinkThenClusterAgain (succ n) vs2 res

     let -- vs = wordsInContext interestingWords ws cs t
         featureWords = S.difference 
                        (S.fromList $ drop 10 $ take 100 commonWords)
                        (S.fromList $ map T.pack $ [".", ":", ","])
         
         vs = wordsInContext3 3 featureWords interestingWords t
     -- vs <- dropSomeWordsX 0.92 vs0
     -- printWordsInContext 100 vs
     let connectedWords = wordsConnectedByContext vs
     res1 <- chineseWhispers 50 $ buildNeighbors connectedWords

     -- res <- shrinkThenClusterAgain 0 vs res1
     let res = res1
     
     writeFile "lexical-new.txt" (show res)
     

printResult :: Assignment -> IO ()
printResult assignments =
     sequence_
       [ do putStrLn ("\n" ++ T.unpack wordclass ++ ":")
            mapM_ print [ word 
                        | (word, cl) <- M.toList assignments
                        , cl == wordclass ]
       | wordclass <- S.toList $ S.fromList $ map snd $ 
                      M.toList assignments ]

testCosineSimilarity =
  do t1 <- readFileIntoChunks "input.txt"
     putStrLn $ "number of text chunks:" ++ show (length t1)
     let t = if False then t1 else shortenChunks 1000000 t1
     reportNumberOfSpacesAndNewlines t
     let commonWords = someCommonWords 8000 t
     let veryCommonWords = take 2000 commonWords
     let targetWords =
           S.difference
           (S.fromList commonWords)
           (S.fromList veryCommonWords)
     let featureWords = S.fromList veryCommonWords
     -- let commonEnds = someCommonEnds 100 t
     let wordOccurences = 
           wordsInContext3 2 featureWords targetWords t
           -- wordsInContext targetWords veryCommonWords commonEnds t
           :: M.Map Context (M.Map T.Text Int)
     -- printWordsInContext 1000 wordOccurences
     let distributionalVectors = 
           M.unionsWith M.union
           [ M.singleton word $ M.singleton ctx count
           | (ctx, wordmap) <- M.toList wordOccurences
           , (word, count) <- M.toList wordmap ]
     let cosineSimilarities =
           catMaybes
           [ let co = cosine afeatures bfeatures
                 sim = oneDiv (1 - co) -- ** 3
             in if sim > 2 -- 9
                then Just ((a,b), sim)
                else Nothing
           | (a, afeatures) <- M.toList distributionalVectors
           , (b, bfeatures) <- M.toList distributionalVectors
           , a < b ]
     -- mapM_ print $ L.sortBy (comparing snd) cosineSimilarities
     putStrLn $ "number of edges: " ++ show (length cosineSimilarities)
     let graph =
           M.unionsWith (++)
           [ M.fromList [ (a, [(b, s)])
                        , (b, [(a, s)])]
           | ((a,b), s) <- cosineSimilarities ]
     res <- chineseWhispers 30 graph
     printResult res
     writeFile "lexical-new.txt" (show res)
     


cosine :: M.Map Context Int -> M.Map Context Int -> Double
cosine a b = prod / len a / len b
  where
    prod = fromIntegral $ sum
           [ a M.! feature * b M.! feature
           | feature <- S.toList $ S.intersection (M.keysSet a) (M.keysSet b)]
    len = sqrt . sum . map ((**2) . fromIntegral . snd) . M.toList


oneDiv x | x < 1e-6 = 1e6
         | otherwise = 1/x

data XFeature = XF T.Text T.Text T.Text T.Text
              | X2 T.Text T.Text
                  deriving (Eq, Ord, Show)
instance NFData XFeature where                      
  rnf (XF x1 x2 x3 x4) = 
    rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4
  rnf (X2 x1 x2) = 
    rnf x1 `seq` rnf x2

testFindFeatures =
  do chunks <- readFileIntoChunks "input.txt"
     mapM_ print $ take 1000 $ findFeatures chunks
     
findFeatures :: TextChunks -> [(XFeature, Int)]
findFeatures chunks =
  reverse $ L.sortBy (comparing snd) $ 
  M.toList $ M.map S.size $
  mr (M.unionWith S.union) ex chunks
  where ex chunk = chunkedBuildMapOfSets 1000
                   --- [ (XF (we a) (we b) (we c) (we d), 1) 
                   --- | (a:b:_:c:d:_) <- L.tails $ wordsOfChunk chunk ]
                   [ (X2 a b, S.singleton c)
                   | (a:c:b:_) <- L.tails $ wordsOfChunk chunk ]
        we = T.reverse . T.take 2 . T.reverse


chunkedBuildMapAdd n pairs = rec $ map M.fromList $ chunks pairs
  where chunks [] = []
        chunks list = let (a,b) = splitAt 5000 list
                      in a : chunks b
        rec = xfoldl1 (\a b-> lim (M.unionWith (+) a b))
        lim = M.fromList . take n . reverse 
              . L.sortBy (comparing snd) . M.toList

chunkedBuildMapOfSets n pairs = rec $ map M.fromList $ chunks pairs
  where chunks [] = []
        chunks list = let (a,b) = splitAt 1000 list
                      in a : chunks b
        rec = xfoldl1 (\a b-> lim (M.unionWith S.union a b))
        lim = M.fromList . take n . reverse 
              . L.sortBy (comparing $ S.size . snd) . M.toList

main =
  -- buildLexicalClusters
  testCosineSimilarity
  -- testFindFeatures