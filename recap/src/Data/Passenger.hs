module Data.Passenger where

data Initials = Initials {firstName :: !String, surName :: !String}

instance Show Initials where
  show (Initials name surName) = name ++ " " ++ surName

data Animal = Animal {species :: !String, nickname :: !String} deriving (Show)

data Passenger
  = PInitials !Initials
  | PAnimal !Animal
  | PThing {url :: !String}

instance Show Passenger where
  show (PInitials initials) = show initials
  show (PAnimal animal) = show animal
  show (PThing url) = "Please, visit " ++ url

name :: Passenger -> String
name passenger = case passenger of
  PInitials initials -> firstName initials
  PAnimal animal -> nickname animal
  PThing url -> error "no nickname"
