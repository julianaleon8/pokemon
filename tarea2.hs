import Data.List
import Data.Maybe
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
esHeap (ArbolRosa y xs) = foldl (&&) True (map (y>=))

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
	      | x > 0	= Just (foldl (*) 1 [1..x], x-1)
	      | otherwise = Nothing


--factorizar :: Int -> [Int]

	   


