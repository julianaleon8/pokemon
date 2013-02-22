
import System.IO
import System.Environment
import Data.List
import qualified Data.Map as Map
import Data.Maybe

data TipoPokemon 
     = Bug
     | Dark
     | Dragon
     | Electric
     | Fighting
     | Fire
     | Flying
     | Ghost
     | Grass
     | Ground
     | Ice
     | Normal	
     | Poison 
     | Psychic
     | Rock
     | Steel
     | Water
     deriving (Bounded,Eq,Enum,Read,Show)    
data Ataque = Ataque { nombreAtaque :: String
                       , tipoAtaque :: TipoPokemon
                       , fisico :: Bool
                       , pps :: Int
                       , poder :: Int
                     } deriving (Show,Read)
                             
                                 
data Estadistica = Estadistica { vida :: Int 
                               , ataque :: Int 
                               , defensa :: Int
                               , ataqueEspecial :: Int
                               , defensaEspecial :: Int  
                               , velocidad :: Int 
                               , valorIndividual :: Int 
                               , valorEsfuerzo :: Int
                               } deriving (Show)

data Especie = Especie { numeroCatalogo :: Int
                       , nombreEspecie :: String
                       , tipoPokemon :: TipoPokemon
		       , tipoPokemon1 :: Maybe TipoPokemon
                       , estadisti :: Estadistica
                       , preEvolucion :: Maybe Int
                       , evolucion :: Maybe String
                       } deriving (Show)
                                  
data Monstruo = Monstruo { especie :: Int
                         ,sobreNombre :: String
                         ,nivel :: Int 
                         ,hp :: Int
                         ,ataque1 :: Ataque
			 ,ataque2 :: Maybe Ataque
			 ,ataque3 :: Maybe Ataque
			 ,ataque4 :: Maybe Ataque
                         }deriving (Show)
                                   
relacionAtaqueTipo :: TipoPokemon      -- Tipo de ataque a determinar la relación.
                   -> ( [TipoPokemon]  -- Tipos super efectivos a el (2x dano). 
                      , [TipoPokemon]  -- Tipos resistentes a el (0.5x dano).
                      , [TipoPokemon]  -- Tipos inmunes a el (0x dano).
                      )
relacionAtaqueTipo x
  | Bug      <- x = ([Grass, Psychic, Dark], [Fighting, Flying, Poison, Ghost, Steel, Fire], [])
  | Dark     <- x = ([Ghost, Psychic], [Fighting, Steel, Dark], [])
  | Dragon   <- x = ([Dragon], [Steel], [])
  | Electric <- x = ([Flying, Water], [Grass, Electric, Dragon], [Ground])
  | Fighting <- x = ([Normal, Rock, Steel, Ice, Dark], [Flying, Poison, Bug, Psychic], [Ghost])
  | Fire     <- x = ([Bug, Steel, Grass, Ice], [Rock, Fire, Water, Dragon], [])
  | Flying   <- x = ([Fighting, Bug, Grass], [Rock, Steel, Electric], [])
  | Ghost    <- x = ([Ghost, Psychic], [Steel, Dark], [Normal])
  | Grass    <- x = ([Ground, Rock, Water], [Flying, Poison, Bug, Steel, Fire, Grass, Dragon], [])
  | Ground   <- x = ([Poison, Rock, Steel, Fire, Electric], [Bug, Grass], [Flying])
  | Ice      <- x = ([Flying, Ground, Grass, Dragon], [Steel, Fire, Water], [])
  | Normal   <- x = ([], [Rock, Steel], [Ghost])
  | Poison   <- x = ([Grass], [Poison, Ground, Rock, Ghost], [Steel])
  | Psychic  <- x = ([Fighting, Poison], [Steel, Psychic], [Dark])
  | Rock     <- x = ([Flying, Bug, Fire, Ice], [Fighting, Ground, Steel], [])
  | Steel    <- x = ([Rock, Ice], [Steel, Fire, Water, Electric], [])
  | Water    <- x = ([Ground, Rock, Fire], [Water, Grass, Dragon], [])

maxHp especie nivel =  (div (((2*(vida (estadisti especie)))+31+ (div 255 4)+100)* nivel) 100) +10
                                               
-- ver aqui lo del numero de la especie 
estadisticas  nivel especie func = (div ((31+(2*(func (estadisti especie))+(div 31 4)))*nivel) 100) +5 
                                                           

efectoAtaque = 10

--dano pokemonAta pokemonDefensa numeroAtaque = ((( ( (2*nivel pokemonAta)/5 +2)*numeroAtaque*(ataques numeroAtaque/defensa pokemonDefensa)/50) ) )*modificador 
-- modificador ( a d ) * modificador (a dd )  
-- falta ponerlo para los dos tipos de defensa y multiplacarlo

modificador ataque defensa  
  | elem defensa (ptrestupla (relacionAtaqueTipo ataque)) = 2
  | elem defensa (strestupla (relacionAtaqueTipo ataque)) = 0.5
  | elem defensa (ttrestupla (relacionAtaqueTipo ataque)) = 0
  | otherwise = 1
                
ptrestupla (a,_,_)= a
strestupla (_,a,_)= a
ttrestupla (_,_,a)= a

{-splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f l@(x:xs)
  | f x = splitOn f xs
  | otherwise = let (h,t) = break f l in h:(splitOn f t)
-}

aux [] = []
aux xs = fst g:[] ++ aux (drop 1 (snd g))
    where g = break (','==) xs


--numero, nombre, tipo1, tipo2, hp, ataque, defensa, ataque_especial, defensa_especial, velocidad, padre, evolucion

--tuplas :: [a] -> (b,c)

toInt :: String -> Int
toInt a = read a :: Int

toEstadistica :: [String] -> Estadistica

toEstadistica xs =  Estadistica { vida=toInt(head xs)
	      	    		, ataque = toInt(xs !! 1)
				, defensa = toInt(xs !! 2)
				, ataqueEspecial = toInt(xs !! 3)
				, defensaEspecial = toInt(xs !! 4)
				, velocidad = toInt(xs !! 5)
				, valorIndividual = 31
				, valorEsfuerzo = 255 }

preEvoluc :: String -> Maybe Int
preEvoluc "" = Nothing
preEvoluc a = Just (toInt a)
 
evoluc :: String -> Maybe String
evoluc "" = Nothing
evoluc a = Just a

tipoPok :: String -> Maybe TipoPokemon
tipoPok "" = Nothing
tipoPok a = Just (read a :: TipoPokemon)

--ata :: String -> Maybe Ataque
--ata "" = Nothing
--ata a = Just (read a :: Ataque)

tuplasEspecie :: [String] -> (Int, Especie)
tuplasEspecie xs = (toInt(head xs), g xs)
       where
	g xs = Especie { numeroCatalogo=toInt (head xs)
	       	       , nombreEspecie= xs !! 1
		       , tipoPokemon = read (xs !! 2) :: TipoPokemon
		       , tipoPokemon1 = x (xs !! 3)
		       , estadisti = toEstadistica (take 6 (drop 4 xs))
		       , preEvolucion = preEvoluc (xs !! 10)
		       , evolucion = h preEvolucion xs }
	h preEvolucion xs
	  | isNothing (preEvoluc (xs !! 10)) = Nothing
	  | otherwise = Just (xs !! 11) 
	x y 
	  | y == "" = Nothing
	  | otherwise = Just (read y :: TipoPokemon)
	
tuplasAtaque :: [String] -> (String, Ataque)
tuplasAtaque xs = (head xs, g xs)
	     where
		g xs = Ataque { nombreAtaque=(head xs)
		       	      , tipoAtaque=read(xs !! 1) :: TipoPokemon
			      , fisico=read(xs !! 2) :: Bool
			      , pps=toInt(xs !! 3)
			      , poder=toInt(xs !! 4) }

tuplasMonstruo :: [String] -> (String, Monstruo)
tuplasMonstruo xs = (head xs, g xs)
	       where
		g xs = Monstruo { especie=toInt (head xs)
		       		, sobreNombre = (xs !! 1)
				, nivel=toInt (xs !! 2)
				, hp = 1
				, ataque1 = read (xs !! 3) :: Ataque
				, ataque2 = x (xs !! 4)
				, ataque3 = x (xs !! 5)
				, ataque4 = x (xs !! 6) }
		x y 
	  	  | y == "" = Nothing
	  	  | otherwise = Just (read y :: Ataque)

	--	calculaHP data esp = 
{-info n e1 = print ("Numero Catalogo: " ++ (head g) ++ "Nombre especie: " ++ (g !! 1) ++ Tipo Pokemon: " ++ (g !! 2) ++ (h (g !! 3)) ++ "Estadisticas: " ++ (imprimeEstadistica (snd e1))++ (h (g !! 10)) ++ (h (g !! 11))) 
       	  where
		g = snd e1
		h x
		  | x == Nothing = ""
		  | otherwise = fromJust(x)

imprimeEstadistica a = print ("HP: " ++ (head xs) ++ "Ataque: " ++ (a !! 1) ++)-}

--getEstadistica :: Especie -> Estadistica
--getEstadistica = Estadistica

getHP :: Estadistica -> Int
getHP = vida

main =  do especies <- readFile "especies.txt" 
     	   ataques <- readFile "ataques.txt"
	   entrenador1 <- readFile "entrenador1.txt" 
	   --entrenador2 <-readFile "entrenador2.txt"
     	   let esC = lines especies
	   let esA = map aux esC
	   let esB = map tuplasEspecie esA
	   let esD = Map.fromList esB
	   let atC = lines ataques
	   let atA = map aux atC
	   let atB = map tuplasAtaque atA
	   let atD = Map.fromList atB
	   let e1C = lines entrenador1
	   let e1A = map aux e1C
	   let e1B = map tuplasMonstruo e1A
	   let e1D = Map.fromList e1B 
	   print (esB)
