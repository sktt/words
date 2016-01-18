import Data.List (unfoldr, tails)
import System.Random (randomRIO, newStdGen, StdGen(..), random)
import Data.Text (pack, unpack, replace)

----
-- This thing randomizes a text using the input text `input`. Like markov chains
--
-- It uses the first 5 chars as a start `window` and then randomly selects one
-- of the possible following characters (given input). Continues until 2000 chars
-- are there or the thing crashes because there are no possiible candidates for
-- next char for the given `window` causing 0 mod 0.
--
-- ex: $ ./words < textFile
----

-- n last elements of array
nLast :: Int -> [a] -> [a]
nLast n xs = foldl (const . tail) xs (drop n xs)

-- array of possible preceding characters after `window` given `input`
candidates :: Eq a => [a] -> [a] -> [a]
candidates input curr = (map head -- get the first preceding el for every occurence
  . filter (not . null) -- filter empty
  . map (drop wl) -- drop the the part that is already in `window`
  . filter ((window ==) . take wl) -- find where `window` exists in `input`
  . tails) input
  where wl = length window
        window = nLast 5 curr

-- recursively append next char
generate :: Eq a => [a] -> [a] -> StdGen -> [[a]]
generate input start g = map snd (unfoldr lam (g, start))
  where lam (g, curr) = Just ((g, curr), appendNextEl curr (nextEl (g, candidates input curr)))

appendNextEl :: [b] -> (a, b) -> (a, [b])
appendNextEl a (b, c) = (b, a ++ [c])

nextEl :: (StdGen, [a]) -> (StdGen, a)
nextEl (g, candidates) = (g', candidates !! mod r (length candidates))
  where (r, g') = random g :: (Int, StdGen)

main = do
  input <- getContents
  g <- newStdGen
  let start = take 5 input
  putStrLn $ last . take 2000 $ generate input start g
