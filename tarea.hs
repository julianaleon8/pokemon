import List (insert)
import Data.List (partition)
data ArbolRosa a = ArbolRosa { elemento :: a
                             ,hijos :: [ArbolRosa a]
                   }deriving (Show, Read)
                             
                                  
--1)
insertionSort :: (Ord a) => [a]-> [a]
insertionSort  xs =  foldl (flip insert) [] xs    

partition :: (a -> Bool) -> [a] -> ([a], [a])
--partition _ [] = ([],[]) 
--partition p (x:xs) = 
partition p xs = foldr (select p) ([],[]) xs

select :: (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
select p x ~(ts,fs) | p x       = (x:ts,fs)
                    | otherwise = (ts, x:fs)

--2)

altura :: ArbolRosa a -> Int
altura (ArbolRosa _ []) = 0 
altura (ArbolRosa _ xs) = 1 + maximum (map (altura) xs)


sumaArbol :: Num a => ArbolRosa a -> a
sumaArbol (ArbolRosa a []) = a 
sumaArbol (ArbolRosa a  xs) =  foldl (+) a (map (sumaArbol) xs) 

aplanar :: ArbolRosa a -> [a]
aplanar  (ArbolRosa a []) = [a]
aplanar  (ArbolRosa x xs) =  x : concat (map (aplanar) xs)

mapRosa:: (a->b)-> ArbolRosa a -> ArbolRosa b
mapRosa  f (ArbolRosa e [] ) =(ArbolRosa (f e) [])
mapRosa f (ArbolRosa e xs) = ArbolRosa (f e) (map (mapRosa f) xs) 

esHeap :: Ord a => ArbolRosa a -> Bool
esHeap (ArbolRosa e []) = True
esHeap (ArbolRosa e (x:xs)) = False


--3)

--foldRosa :: (a-> b-> a)-> a -> ArbolRosa a -> a
