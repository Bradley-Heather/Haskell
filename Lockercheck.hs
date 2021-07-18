
import qualified Data.Map as Map
import           Control.Monad    (liftM, liftM2, mapM)
import           Data.Char        (toUpper)
import           System.Directory ()
import           System.IO        (Handle, IOMode (ReadMode), hGetLine, hIsEOF, withFile)
import           System.IO.Error  (catchIOError) 

data LockerState = Taken | Free 
  deriving (Show, Eq)

type Code = String 

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code

lockerLookup lockerNumber map = 
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
            then Right code
            else Left $ "Locker " ++ show lockerNumber ++ " is already Taken" 

lockers :: LockerMap

lockers = Map.fromList
    [(100, (Taken, "zd392"))
    ,(101, (Free, "an421"))
    ,(102, (Free, "bh672"))
    ,(103, (Free, "xy673"))
    ,(104, (Taken, "rn763"))
    ]

data Interaction = 
    Question String Interaction Interaction
    | Result String
    deriving (Show, Eq) 

askBool :: String -> IO Bool 
askBool question = do
    putStrLn (question ++ "Y/N")
    x <- getChar 
    putStrLn ""
    return (x `elem` "Yy")

interaction :: Interaction -> IO ()
interaction (Question q y n) = do
    b <- askBool q
    if b then interaction y else interaction n
interaction (Result r) = putStrLn r     


addLocker :: Interaction
addLocker =
    Question "Would you like to add a locker?"
         (Question "What will it's code be?")
            Result "Great, I've added it"
         (Result "No problem.")


