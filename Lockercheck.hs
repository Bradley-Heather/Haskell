import qualified Data.Map as Map

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

addLocker