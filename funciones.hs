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
