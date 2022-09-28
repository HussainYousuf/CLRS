-- Countdown
import System.Environment (getArgs)
import Data.List
import System.IO

data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

data Expr = Val Int | App Op Expr Expr 

instance Show Expr where 
    show (Val n) = show n
    show (App o e1 e2) = show' e1 ++ show o ++ show' e2 
        where 
            show' (Val n) = show n
            show' e = "(" ++ show e ++ ")"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Mul x y = x <= y && x /= 1 && y /= 1
valid Sub x y = x > y
valid Div x y = x `mod` y == 0 && y /= 1

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Mul x y = x * y
apply Sub x y = x - y
apply Div x y = x `div` y

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o e1 e2)  = [apply o x y | x <- eval e1, y <- eval e2, valid o x y]

split :: [a] -> [([a], [a])]
split [] = []
split xs = init $ tail $ zip inits' tails'
    where
        inits' = inits xs
        tails' = tails xs

ops :: [Op] 
ops = [Add, Sub, Mul, Div]

choices :: [a] -> [[a]]
choices = concat . map permutations . subsequences  

combine :: [Int] -> [(Expr, Int)]
combine [n] = [(Val n, n) | n > 0]
combine ns = [(App o e1 e2, apply o x y) | o <- ops, (ls, rs) <- split ns, (e1, x) <- combine ls, (e2, y) <- combine rs, valid o x y]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | xs <- choices ns, (e, x) <- combine xs, n == x]

countdown :: IO ()
countdown = mapM_ (putStrLn . show) (solutions [1,3,7] 14)

hangman :: IO ()
hangman = do
    putStrLn "Enter a word"
    word <- sgetLine
    putStrLn $ map (\_ -> '_') word
    play word

sgetLine :: IO String
sgetLine = do 
    hSetEcho stdin False
    word <- getLine
    hSetEcho stdin True
    return word

play :: String -> IO ()
play word = do
    putStrLn "Try to guess it"
    guess <- getLine
    putStrLn [if c `elem` guess then c else '_' | c <- word]
    if guess == word then putStrLn "You Won" else play word

main = do
    args <- getArgs
    print args