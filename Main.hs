import Hledger
import Data.List
import Data.Function
import Data.Ord
import Data.Maybe
import System.Environment

data PostingWithPath = PostingWithPath {
                     ppposting :: Posting,
                     ppjournalidx :: Int,
                     pptxnidx :: Int,
                     pppidx :: Int }
                 deriving (Show)

instance Eq PostingWithPath where
    a == b = ppjournalidx a == ppjournalidx b
          && pptxnidx a == pptxnidx b
          && pppidx a == pppidx b

allPostingsWithPath :: Int -> Journal -> [PostingWithPath]
allPostingsWithPath jidx j = do
    (txnidx, txn) <- zip [0..] $ jtxns j
    (pidx, p) <- zip [0..] $ tpostings txn
    return $ PostingWithPath { ppposting = p,
      ppjournalidx = jidx, pptxnidx = txnidx, pppidx = pidx }

binBy :: Ord b => (a -> b) -> [a] -> [[a]]
binBy f = groupBy ((==) `on` f) . sortBy (comparing f)

matching :: [PostingWithPath] -> [(PostingWithPath, PostingWithPath)]
matching pps = do
    bin <- binBy (aquantity.head.amounts.pamount.ppposting) pps
    let left = filter ((0 ==) . ppjournalidx) bin
    let right = filter ((1 ==) . ppjournalidx) bin
    zip left right -- FIXME

readJournalFile' :: FilePath -> IO Journal
readJournalFile' fn = do
    Right j <- readJournalFile (Just "journal") Nothing fn
    return j

main :: IO ()
main = do
  [acct, f1, f2] <- getArgs

  j1 <- readJournalFile' f1
  j2 <- readJournalFile' f2

  let pp1 = filter ((== acct) . paccount . ppposting) (allPostingsWithPath 0 j1)
  let pp2 = filter ((== acct) . paccount . ppposting) (allPostingsWithPath 1 j2)

  let m = matching (pp1 ++ pp2)

  let pp1' = pp1 \\ map fst m
  let pp2' = pp2 \\ map snd m

  let unclearedtxn1 = map (fromJust . ptransaction . ppposting) $ nubBy ((==) `on` pptxnidx) pp1'
  let unclearedtxn2 = map (fromJust . ptransaction . ppposting) $ nubBy ((==) `on` pptxnidx) pp2'

  putStrLn "Uncleared transactions in the first journal:\n"
  mapM_ (putStr . show) unclearedtxn1

  putStrLn "Uncleared transactions in the second journal:\n"
  mapM_ (putStr . show) unclearedtxn2
