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


















data Tree a =
    Node (Tree a) a (Tree a)
  | Leaf
  deriving (Show)

buildTree :: a -> Int -> Tree a
buildTree x h =
    if h <= 0 
        then Leaf 
        else
            let subTree = buildTree x (h - 1)
            in Node subTree x subTree

labelTree :: Tree a -> Tree (Int, a)
labelTree tree = 
    fst (runWithCounter (labelTree' tree) 1)

newtype WithCounter a = MkWithCounter { runWithCounter :: Int -> (a, Int) }

labelTree'Orig :: Tree a -> WithCounter (Tree (Int , a))
labelTree'Orig (Node l x r) =
    MkWithCounter (\ currentLabel ->
    case runWithCounter (labelTree'Orig l) currentLabel of
        (l', currentLabel') ->
            case runWithCounter tick currentLabel' of
                (labelForX, nextLabel) ->
                  case runWithCounter (labelTree'Orig r) nextLabel of
                       (r', currentlabel'') ->
                         (Node l' (labelForX, x) r', currentlabel'')
    )

labelTree'Orig Leaf =
    MkWithCounter (\ currentLabel -> (Leaf, currentLabel))

labelTree' :: Tree a -> WithCounter (Tree (Int , a))
labelTree' (Node l x r) =
    labelTree' l `bindWithCounter` \l' -> 
    tick `bindWithCounter` \labelForX  ->
    labelTree' r `bindWithCounter` \r' ->
    returnWithCounter (Node l' (labelForX, x) r')

labelTree' Leaf =
    returnWithCounter Leaf

tick :: WithCounter Int 
tick = 
    MkWithCounter (\ current -> (current, current + 1))

bindWithCounter :: WithCounter a -> (a -> WithCounter b) -> WithCounter b
bindWithCounter computation continuation =
    MkWithCounter (\ currentCounter ->
        case runWithCounter computation currentCounter of 
           (result, currentCounter') -> runWithCounter (continuation result) currentCounter'
    )

returnWithCounter :: a -> WithCounter a
returnWithCounter x =
    MkWithCounter(\ currentCounter -> (x, currentCounter))