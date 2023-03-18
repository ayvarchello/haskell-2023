module Data.Direction where

import Data.Spreadsheet (Index)

data Direction = DUp | DDown | DLeft | DRight deriving (Read)

-- (0.5 балла) Напишите функцию, сдвигающую курсор в нужном направлении.

updatePosition :: Direction -> Index -> Index
updatePosition = error "updatePosition is not defined"
