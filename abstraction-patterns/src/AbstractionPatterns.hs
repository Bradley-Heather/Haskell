module AbstractionPatterns where

import           Data.Map (Map)
import qualified Data.Map as M 

newtype Address = MkAddress Int 
  deriving (Eq, Ord, Show)

addressMapping :: Map Address Address 
addressMapping =
  M.fromList 
    [ (MkAddress 3, MkAddress 7)
    , (MkAddress 4, MkAddress 20)
    , (MkAddress 5, MkAddress 3)
    , (MkAddress 7, MkAddress 14)
    , (MkAddress 9, MkAddress 5)
    , (MkAddress 16, MkAddress 9)
    ]

threeHopsOriginal :: Address -> Maybe String
threeHopsOriginal address0 = 
    case M.lookup address0 addressMapping of 
        Nothing -> Nothing
        Just address1 -> 
            case M.lookup address1 addressMapping of 
                Nothing -> Nothing 
                Just address2 ->
                    case M.lookup address2 addressMapping of
                        Nothing -> Nothing 
                        Just address3 -> Just (show address3)    

threeHops :: Address -> Maybe String
threeHops address0 = 
    M.lookup address0 addressMapping `bindMaybe` \ address1 -> 
    M.lookup address1 addressMapping `bindMaybe` \ address2 ->
    M.lookup address2 addressMapping `bindMaybe` \ address3 -> 
    Just (show address3) 
       

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe computation continuation =
    case computation of 
        Nothing -> Nothing 
        Just result -> continuation result 