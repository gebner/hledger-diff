import Hledger
import Data.List
import Data.Function
import Data.Ord
import Data.Maybe
import System.Environment
import System.Exit
import Data.Time
import Data.Either
import qualified Data.Text as T

data PostingWithPath = PostingWithPath {
                     ppposting :: Posting,
                     pptxnidx :: Int,
                     pppidx :: Int }
                 deriving (Show)

instance Eq PostingWithPath where
    a == b = pptxnidx a == pptxnidx b
          && pppidx a == pppidx b

pptxn :: PostingWithPath -> Transaction
pptxn = fromJust . ptransaction . ppposting

ppamountqty :: PostingWithPath -> Quantity
ppamountqty = aquantity . head . amounts . pamount . ppposting

allPostingsWithPath :: Journal -> [PostingWithPath]
allPostingsWithPath j = do
    (txnidx, txn) <- zip [0..] $ jtxns j
    (pidx, p) <- zip [0..] $ tpostings txn
    return PostingWithPath { ppposting = p, pptxnidx = txnidx, pppidx = pidx }

binBy :: Ord b => (a -> b) -> [a] -> [[a]]
binBy f = groupBy ((==) `on` f) . sortBy (comparing f)

combine :: ([a], [b]) -> [Either a b]
combine (ls, rs) = map Left ls ++ map Right rs

combinedBinBy :: Ord b => (a -> b) -> ([a], [a]) -> [([a], [a])]
combinedBinBy f = map partitionEithers . binBy (either f f) . combine

greedyMaxMatching :: (Eq a, Eq b) => [(a,b)] -> [(a,b)]
greedyMaxMatching = greedyMaxMatching' []

greedyMaxMatching' :: (Eq a, Eq b) => [Either a b] -> [(a,b)] -> [(a,b)]
greedyMaxMatching' alreadyUsed ((l,r):rest)
  | Left l `elem` alreadyUsed || Right r `elem` alreadyUsed
      = greedyMaxMatching' alreadyUsed rest
  | otherwise = (l,r) : greedyMaxMatching' (Left l : Right r : alreadyUsed) rest
greedyMaxMatching' _ [] = []

dateCloseness :: (PostingWithPath, PostingWithPath) -> Integer
dateCloseness = negate . uncurry (diffDays `on` tdate.pptxn)

type Matching = [(PostingWithPath, PostingWithPath)]

matching :: [PostingWithPath] -> [PostingWithPath] -> Matching
matching ppl ppr = do
    (left, right) <- combinedBinBy ppamountqty (ppl, ppr) -- TODO: probably not a correct choice of bins
    greedyMaxMatching $ sortBy (comparing dateCloseness) [ (l,r) | l <- left, r <- right ]

readJournalFile' :: FilePath -> IO Journal
readJournalFile' fn = do
    Right j <- readJournalFile (Just "journal") Nothing False fn
    return j

matchingPostings :: AccountName -> Journal -> [PostingWithPath]
matchingPostings acct j = filter ((== acct) . paccount . ppposting) $ allPostingsWithPath j

pickSide :: Side -> (a,a) -> a
pickSide L (l,_) = l
pickSide R (_,r) = r

unmatchedtxns :: Side -> [PostingWithPath] -> Matching -> [Transaction]
unmatchedtxns s pp m =
    map pptxn $ nubBy ((==) `on` pptxnidx) $ pp \\ map (pickSide s) m

main :: IO ()
main = getArgs >>= \args -> case args of
  [acct, f1, f2] -> diffCmd (T.pack acct) f1 f2
  ["--version"] -> putStrLn "hledger-diff 0.2.0.9"  -- keep synced with hledger-diff.cabal
  _ -> do
    putStrLn "Usage: hledger-diff account:name left.journal right.journal"
    exitFailure

diffCmd :: AccountName -> FilePath -> FilePath -> IO ()
diffCmd acct f1 f2 = do
  j1 <- readJournalFile' f1
  j2 <- readJournalFile' f2

  let pp1 = matchingPostings acct j1
  let pp2 = matchingPostings acct j2

  let m = matching pp1 pp2

  let unmatchedtxn1 = unmatchedtxns L pp1 m
  let unmatchedtxn2 = unmatchedtxns R pp2 m

  putStrLn "Unmatched transactions in the first journal:\n"
  mapM_ (putStr . show) unmatchedtxn1

  putStrLn "Unmatched transactions in the second journal:\n"
  mapM_ (putStr . show) unmatchedtxn2
