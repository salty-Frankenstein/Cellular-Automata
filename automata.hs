import Control.Monad.State
import System.Process
import System.Random

type Grid = [[Int]]

neighbour :: Grid -> (Int, Int) -> Int
neighbour g (i,j) = sum [ g!!x!!y | x <- [i-1..i+1], y <- [j-1..j+1]] - (g!!i!!j)

isAlive :: Grid -> (Int, Int) -> Int
isAlive g (i,j) = 
    case neighbour g (i,j) of
        3 -> 1
        2 -> g!!i!!j
        _ -> 0

getNext :: Grid -> Grid
getNext g = let i = length g; j = length $ head g in
    [
        [   if x == 0 || y == 0 || x == i-1 || y == j-1 
            then 0
            else isAlive g (x,y) 
            | y <- [0..j-1] 
        ] 
        | x <- [0..i-1] 
    ]

nextStateIO :: StateT Grid IO Grid
nextStateIO = StateT $ \g -> return (g, getNext g)

printList :: Show a => [a] -> IO ()
printList [] = return ()
printList (x:xs) = do
    print x
    printList xs

run :: StateT Grid IO ()
run = do
    liftIO $ system "cls"
    s <- nextStateIO
    liftIO $ printList s

getGrid :: Int -> Int -> [Int] -> Grid
getGrid x y l = let zero = take (y+2) $ repeat 0 in 
    [zero] ++ map (\x->[0]++x++[0]) [ [ l!!(i*y+j) | j <- [0..y-1]] | i <- [0..x-1]] ++ [zero]

randomBool :: IO [Int]
randomBool = do
    gen <- newStdGen
    return $ randomRs ((0, 1)::(Int, Int)) gen

randomGrid :: Int -> Int -> IO Grid
randomGrid x y = do
    s <- liftM (take (x*y)) randomBool
    return $ getGrid x y s

initial :: IO Grid
initial = randomGrid 20 20

bindStream :: StateT Grid IO a -> (a, Grid) -> IO (a, Grid)
bindStream run = \(_,l) -> runStateT run l >>= bindStream run

foreverDo :: StateT Grid IO a -> IO (a, Grid)
foreverDo run = do
    g <- initial
    runStateT run g >>= bindStream run

main :: IO ()
main = do
    foreverDo run
    return ()
