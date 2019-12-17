import System.Random

getSeq :: [Int]
getSeq = [x | x <- [1..10]]

binSearch :: [Int] -> Int -> Int -> Int -> Int -> Int
binSearch vect num lim_low lim_high attempts
    | vect !! middle > num = binSearch vect num lim_low middle (attempts + 1)
    | vect !! middle < num = binSearch vect num middle lim_high (attempts + 1)
    | otherwise = attempts
    where
        middle = div (lim_low + lim_high) 2

main :: IO ()
main = do
    num <- randomRIO (1, (length getSeq))
    putStrLn ("Number drawn: " ++ (show num))
    putStrLn ("Number of attempts: " ++ (show (binSearch getSeq num 0 (length getSeq) 1)))
