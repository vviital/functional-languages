{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
 
import Data.List
 
class ListOrAtom a where
    isList :: a -> Bool 
    printValue :: a -> String
 
instance ListOrAtom Int where
    isList = const False
    printValue a = show a
 
instance ListOrAtom Double where
    isList = const False
    printValue a = show a
 
instance ListOrAtom Char where
    isList = const False
    printValue a = show a
 
instance ListOrAtom [Int] where
    isList = const True
    printValue x = show x

instance ListOrAtom [Double] where
    isList = const True
    printValue x = show x

instance ListOrAtom [Char] where
    isList = const True
    printValue x = show x

data ListItem = forall a. ListOrAtom a => MkItem a
 
instance ListOrAtom ListItem where
    isList (MkItem a) = isList a
    printValue (MkItem a) = printValue a
 
pack :: ListOrAtom a => a -> ListItem
pack = MkItem

list ::[ListItem]
list = [ pack (1 :: Int)
           , pack "foo"
           , pack ([1,2,3] :: [Double])
           , pack 'c'
           , pack (pack (42 :: Int))
           , pack ([4,5,6] :: [Int])
           ]

showAll x = map (\x -> printValue x) x

solve l = showAll((filter (\x -> not(isList x)) l) ++ (filter (\x -> isList x) l))
