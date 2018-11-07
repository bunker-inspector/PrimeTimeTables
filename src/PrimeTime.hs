module PrimeTime where
import Data.List -- intercalate
import Data.List.Split -- chunksOf
import Text.Printf -- printf

-- Determines if input is a prime number
-- Complexity: O(sqrt(n))
isPrime :: Integral t => t -> Bool
isPrime t = case t of
    1          -> False -- WE CAN DEBATE BUT WIKIPEDIA SAYS NO
    2          -> True  -- Unique case for even numbers
    t | even t -> False -- Even numbers above 2 cannot be prime, skip
    _ -> all (\x -> (/=0) $ t `mod` x) -- Returns true if x is not evenly divisible by all checks
        $ filter odd -- remove even numbers, can't be disibile by even number
        $ [3 .. (ceiling $ sqrt $ fromIntegral t)] -- Range from 3 to sqrt of input

-- Determines if input is not prime number
-- Complexity: O(sqrt(n))
isNotPrime = not . isPrime

-- Returns a list of the first n prime numbers
-- Complexity: O(n * sqrt(n))
getNPrimes n = take n $ foldr (\x y -> if isPrime x then x:y else y) [] [1..]

-- Returns all pair combinations of two lists
-- Complexity: O(n^2)
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

-- Cartesian product of the first n primes
-- Complexity: O(n)
primePairs n = cartProd p p where p = getNPrimes n

-- Calculate products of all prime number combinations
-- Complexity: O(n)
primeProducts = map (\y -> (fst y) * (snd y)) . primePairs

-- Prepends character c repeatedly to s until s is length i
-- Complexity: O(n)
fill :: Int -> Char -> String -> String
fill i c s = let l = length s
             in case s of
                s | l > i -> s
                _ -> (replicate (i - l) c) ++ s

-- Combines previous functions and formats data into table
primeTimeTable n = do
    let products = map show $ primeProducts n
        maxLen = maximum $ map length products
        fillSpaces = fill maxLen ' '
        filledPrimes = map (fillSpaces . show) $ getNPrimes n
        productLines = map (intercalate " | ") $ chunksOf n $ map fillSpaces products
        lineLength = length (head productLines)
        offset = replicate (maxLen + 1) ' '
        divider = replicate (lineLength+1) '-'

    putStrLn((++)
        (printf "  %s%s\n" offset $ intercalate "   " filledPrimes)
        $ concat
        $ (:)
            (printf "%s|%s-|\n" offset divider)
            (map (\x -> printf "%s | %s |\n%s|%s-|\n" (snd x) (fst x) offset divider) $ zip productLines filledPrimes))


