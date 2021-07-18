module IO where


import     Control.Monad    (liftM, liftM2)
import     Data.Char        (toUpper)
import     System.Directory
import     System.IO        (Handle, IOMode (ReadMode), hGetLine, hIsEOF, withFile)
import     System.IO.Error  (catchIOError) 

getTwoLines :: IO String 
getTwoLines = getLine >> getLine 

getTwoLines' :: IO String 
getTwoLines' = do
    getLine 
    getLine 

duplicateLine :: IO String 
duplicateLine = fmap (\x -> x ++ x) getLine 

shout :: IO String
shout = fmap (map toUpper) getLine 

joinTwoLines :: IO String
joinTwoLines = liftM2 (++) getLine getLine 

flipTwoLines :: IO String
flipTwoLines = liftM2 (flip (++)) getLine getLine 

{- Bind  (>>=) :: IO a -> (a -> IOb) -> IO b -}

shoutBack :: IO()
shoutBack = shout >>= putStrLn 

shoutBackTwice :: IO()
shoutBackTwice = 
    shout >>= \x -> putStrLn x >> putStrLn x 

liftM2' :: (a -> b -> c) -> IO a -> IO b -> IO c
liftM2' f ioa iob =
    ioa  >>= \a ->
    iob  >>= \b -> 
    return (f a b)

liftM2'' :: (a -> b -> c) -> IO a -> IO b -> IO c
liftM2'' f ioa iob = do
    a <- ioa
    f a <$> iob
    
    
greeting :: IO ()
greeting = do  
     putStrLn "What is your name?"  
     name <- getLine 
     putStrLn ("Hello, " ++ name ++ "!")
     putStrLn "Where do you live?"
     loc <- getLine
     let
       answer
         | loc == "South Africa" = "Brilliant!"
         | otherwise             = "Not bad..."
     putStrLn answer

ask :: String -> IO String 
ask q = do
    putStrLn q
    getLine 

askMany :: [String] -> IO [String]
askMany []      = return []
askMany (q: qs) = do 
    answer <- ask q
    answers <- askMany qs
    return (answer : answers)

