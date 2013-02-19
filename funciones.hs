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
                       , maximoAta :: Int
                       , poder :: Int
                     } deriving (Show)
                                
                                
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
                       , estadisti :: Estadistica
                       , preEvolucion :: Maybe Int
                       , evolucion :: Maybe Int
                       } deriving (Show)
                                  
data Monstruo = Monstruo { especie :: Especie
                         ,sobreNombre :: String
                         ,nivel :: Int 
                         ,hp :: Int
                         ,ataques :: Ataque
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

maxHp base nivel = ((31 + (2*base)+ 63.75 +100)*nivel)/100 +10 

estadisticas base nivel = ((31+(2*base)+ 31/4)* nivel)/100 +5


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


