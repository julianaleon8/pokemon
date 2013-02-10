import List (insert)
import Data.List (partition)
data ArbolRosa a = ArbolRosa { elemento :: a
                             ,hijos :: [ArbolRosa a]
                   }deriving (Show, Read)
                                  
--1)
insertionSort :: (Ord a) => [a]-> [a]
insertionSort  xs =  foldl (flip insert) [] xs    

--partition :: (a -> Bool) -> [a] -> ([a], [a])
--partition =  


--2)

--[a,[b,[c,[]d,[]]]]
altura :: ArbolRosa a -> Int
altura (ArbolRosa _ []) = 0 
altura (ArbolRosa _ xs) = 1 + maximum (map (altura) xs)


sumaArbol :: Num a => ArbolRosa a -> a
sumaArbol (ArbolRosa a []) = a 
sumaArbol (ArbolRosa a  xs) =  foldl (+) a (map (sumaArbol) xs) 
