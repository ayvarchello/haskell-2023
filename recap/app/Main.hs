{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Applicative (Applicative (liftA2))
import Data.Foldable (for_)
import Data.Passenger

input :: String -> IO String
input !prompt = putStrLn prompt >> getLine

example = Animal {species = "Polar Bear", nickname = "Claus"}

passengers :: [Passenger]
passengers =
  [ PInitials (Initials "Daniil" "Sagalov"),
    PInitials (Initials "Alexander" "Zalyalov"),
    PInitials (Initials "Alexander" "Levin"),
    PAnimal example
  ]

main :: IO ()
main = do
  initials <-
    liftA2
      Initials
      (input "Please, enter your name")
      (input "Please, enter your surname")
  for_ (PInitials initials : passengers) $ \passenger -> do
    putStrLn (name passenger)
