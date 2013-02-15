--import Data.List (insert)
import Data.Maybe
import Data.Graph.Inductive.Internal.Heap as Heap
data ArbolRosa a = ArbolRosa { elemento :: a
     	       	   	     , hijos	:: [ArbolRosa a]
			     }
		deriving Show

--altura :: ArbolRosa a -> Int
--altura elemento = 0 
--altura [xs] = 1 + maximum (map (altura) xs)

sumaArbol :: Num a => ArbolRosa a -> a
sumaArbol (ArbolRosa a []) = a
sumaArbol (ArbolRosa a xs) = foldl (+) a (map (sumaArbol) xs) 

--aplanar

aplanar :: ArbolRosa a -> [a]
--aplanar elemento = elemento
aplanar (ArbolRosa a []) = a:[]
aplanar (ArbolRosa a xs) = a:concat (map (aplanar) xs)

esHeap :: Ord a => ArbolRosa a -> Bool
esHeap (ArbolRosa y []) = True
esHeap (ArbolRosa y xs) = foldl (&&) True (map (y>=)( (concat) (map (aplanar) xs))) && (foldl (&&) True (map (esHeap) xs))

foldRosa f a (ArbolRosa e xs) = f e (foldl (f) a (concatMap (aplanar) xs)) 

unfold' :: (b -> Maybe(a,b)) -> b -> [a]
unfold' f x 
	| isNothing (f x) = []
	| otherwise = fst (g):unfold' f (snd (g))
	where 
	g = fromJust(f x)


factoriales :: Int -> [Int]
factoriales = unfold' f
	    where
	    f x
	      | x > 0	= Just (g x, x-1)
	      | otherwise = Nothing
	    g x = foldl (*) 1 [1..x]

esDivisible :: Int -> Int-> Bool
esDivisible n m = if m*m < n
	      	  then
			if mod n m == 0
			then True
			else
			esDivisible n (m + 1)
		else False


nPrimos :: Int -> Int -> [Int]
nPrimos n m = [a | a <- [n..m], a /= 1, not (esDivisible a 2)]

factorizar :: Int -> [Int]
factorizar n = 1:(unfold' f n)
	   where     
	   	     f x
		       | x > 1	= Just (g x, div x (g x))
	     	       | otherwise = Nothing
		     g x = h [2..x] x
		     h (y:ys) x
		       | (mod x y == 0) = y
		       | otherwise = h ys x

		
altura :: ArbolRosa a -> Int
altura (ArbolRosa _ []) = 0 
altura (ArbolRosa _ xs) = 1 + maximum (map (altura) xs)

{-aplanar' :: ArbolRosa a -> [a]
aplanar' (ArbolRosa a []) = [a]
aplanar' a = unfold' f a
	   where
	   f a
	    | (null (hijos a) == False) = Just ((elemento) a, )
	    | otherwise = Nothing
-}

tuplas :: [a] -> [(a,a)]
tuplas [] = []
tuplas [a] = []
tuplas (x:y:xs) = (x,y):tuplas xs

lista :: [(a,a)] -> [a]
lista [] = []
lista (x:xs) = fst x: snd x: lista xs


heapSort :: (Ord a) => [a] -> [a]

heapSort n = lista (unfold' f (build (tuplas n)))
	 where
	    f x
	      | isEmpty (g x) = Nothing
	      | otherwise = Just ((pri (h (g x)), (seg (h (g x)))), ter(splitMin (ter (h (g x)))))
	    g n = build(tuplas n)
	    h x = splitMin x
	    pri (c,_,_) = c
	    seg (_,c,_) = c
	    ter (_,_,c) = c

				





	   


