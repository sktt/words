import Data.List (unfoldr, tails)
import System.Random (newStdGen, StdGen(..), random)
import Data.Text (pack, unpack, replace)

----
-- This thing randomizes a text using the input text `seed`. Like markov chains
--
-- It uses the first 5 chars as a start `window` and then randomly selects one
-- of the possible following characters (given seed). Continues until 2000 chars
-- are there or the thing crashes because there are no possiible candidates for
-- next char for the given `window` causing 0 mod 0. 
--
-- ex: $ ./words < textFile
----

-- n last elements of array
nLast :: Int -> [a] -> [a]
nLast n xs = foldl (const . tail) xs (drop n xs)

-- array of possible preceding characters after `window` given `seed`
candidates :: Eq a => [a] -> [a] -> [a]
candidates seed window = (map head -- get the first element after last element in `window`
  . filter (not . null) -- filter empty array
    . map (drop wl) -- drop the the part that is already in `window`
      . filter ((==) window . take wl) -- filter for suffixes starting with `window`
        . tails) seed -- create suffix array of seed
  where wl = length window

-- recursively append next char
generate :: Eq a => [a] -> [a] -> StdGen -> [[a]]
generate seed start g = map snd $ unfoldr lam (g, start)
  where lam (g', curr) = Just ((g', curr), (gNext g', curr ++ [randomElem (candidates seed (win curr)) (r g')]))
        win        = nLast 5
        r          = fst . (random :: StdGen -> (Int, StdGen))
        gNext      = snd . (random :: StdGen -> (Int, StdGen))
        randomElem arr n = arr !! mod n (length arr)

main = do
  seed <- getContents
  g <- newStdGen 
  let start = take 5 seed
  putStrLn $ last . take 5000 $ generate seed start g
