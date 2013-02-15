import Data.List 
import Data.Maybe 
import Data.Graph.Inductive.Internal.Heap(
  Heap(..),findMin,deleteMin, build, toList)
  
data ArbolRosa a = ArbolRosa { elemento :: a
                             ,hijos :: [ArbolRosa a]
                   }deriving (Show, Read)


--(ArbolRosa 5 [ArbolRosa 6 [ArbolRosa 9 []]])
                                  
--1)

insertionSort :: (Ord a) => [a]-> [a]
insertionSort  xs =  foldl (flip insert) [] xs    

partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' x xs = foldr (separar x) ([],[]) xs
                 
separar x b ~(a,c) | x b = (b:a,c) 
                   | otherwise = (a,b:c)
                    
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
esHeap (ArbolRosa y xs) = foldl (&&) True (map (y>=)( (concat) (map (aplanar) xs))) && (foldl (&&) True (map (esHeap) xs))


--3)

--a) La funcion de plegado debe recibir una sola funcion puesto que solo posee un constructor para el tipo
-- si la misma tendria varios constructiores deberia tener una funcion  para cada constructor del tipo 
-- La firma de la funcion del plegado para esta (t1 -> [b] -> b) donde t1 es el tipo del ArbolRosa [b] es una lista de elementos   
-- a los cuales se le aplico la funcion recursivamente y b el resultado de aplicar f a la lista


--b) foldRosa :: (t1 -> [b] -> b) -> t -> ArbolRosa t1 -> b
-- JUSTIFICACION 

foldRosa'' f a (ArbolRosa e xs) = f a (foldl (f) a (concatMap (aplanar) xs))  

--foldRosa' f a (ArbolRosa e xs) = f a foldr f e (map (foldRosa f a) xs) 

foldRosa :: (t1 -> [b] -> b) -> t -> ArbolRosa t1 -> b
foldRosa  f a (ArbolRosa e xs) = f e (map (foldRosa f a) xs) 
  
alturaFold :: ArbolRosa a -> Int
alturaFold a = foldRosa (\x y -> if null y then 0 else 1 + maximum y ) 0 a

sumaArbolFold :: Num a => ArbolRosa a -> a
sumaArbolFold a = foldRosa (\x y -> x + sum y ) 0 a   

aplanaFold :: ArbolRosa a -> [a]
aplanaFold ar@(ArbolRosa a xs) =  foldRosa (\x y -> x:concat y) [] ar


--4)

--a) CHARARA DEBES JUSTIFICAR ESTOOOOO
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


aplanarUnfold :: ArbolRosa a -> [a] 
aplanarUnfold a = unfoldr f [a]
  where
    f [] = Nothing 
    f ((ArbolRosa e sa):xs) = Just (e,xs++sa)
    


lista :: [(a,a)] -> [a]
lista [] = []
lista (x:xs) = fst x: lista xs

tuplas :: [a]-> [(a,a)]
tuplas [] = []
tuplas (x:xs) = (x,x): tuplas xs

heapSort' :: Ord a => [a] -> [a]
heapSort' [] = []
heapSort' a = lista (unfold' f (toList(build (tuplas a))))
 where
    f [] = Nothing 
    f (a:ax)  = Just (a,ax)

    