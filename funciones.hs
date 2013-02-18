type Nombre
     = String
type Vida 
     = Int 
       
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
data Ataque 
     = Nombre Bool Int Int Int  
     deriving (Show)
data Estadistica
     = Vida Int Int Int Int Int Int Int deriving (Show)

data Especie
     = Int String TipoPokemon Estadistica String String 
       deriving (Show)
                
data Monstruo 
     = Especie String Int Vida Int 


maxHp base nivel = ((31 + (2*base)+ 63.75 +100)*nivel)/100 +10 

estadisticas base nivel = ((31+(2*base)+ 31/4)* nivel)/100 +5


dano nivel poder ataque defensa modi= ((( ( (2*nivel)/5 +2)*poder*(ataque/defensa)/50) ) )*modificador modi


modificador modi = 4

