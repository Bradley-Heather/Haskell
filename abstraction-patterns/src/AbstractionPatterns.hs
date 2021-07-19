module AbstractionPatterns where

import           Control.Monad
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

threeHops' :: Address -> Maybe String 
threeHops' address0 = do
    address1 <- M.lookup address0 addressMapping
    address2 <- M.lookup address1 addressMapping
    address3 <- M.lookup address2 addressMapping
    return (show address3)


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
    tick         `bindWithCounter` \labelForX  ->
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


{- 

class Applicative m => Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b 

instance Monad Maybe where
    return = returnMaybe
    (>>=)  = bindMaybe

instance Applicative Maybe where
    pure  = return
    (<*>) = ap 

-}

instance Monad WithCounter where
    return = returnWithCounter
    (>>=)  = bindWithCounter 

instance Applicative WithCounter where
    pure  = return
    (<*>) = ap 

instance Functor WithCounter where 
    fmap = liftM

{- 

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f computation = 
    computation >>= \ a -> return (f a) 

liftM2 :: Monad m => (a -> b -> c ) -> m a -> m b -> m c 
liftM2 f computationa computationb =
    computationa >>= \ a ->
    computationb >>= \ b ->
    return (f a b) 

ap :: Monad m => m (a -> b) -> m a -> m b 
ap computationf computationa =
    computationf >>= \ f ->
    computationa >>= \ a ->
    return (f a)

class Applicative f where
    pure  :: a -> f a 
    (<*>) :: f (a -> b) -> f a -> f b

-}