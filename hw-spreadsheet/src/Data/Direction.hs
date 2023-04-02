module Data.Direction where

import Data.Spreadsheet (Index)
import Data.Char (isLatin1)

data Direction = DUp | DDown | DLeft | DRight deriving (Read)

-- (0.5 балла) Напишите функцию, сдвигающую курсор в нужном направлении.

updatePosition :: Direction -> Index -> Index
updatePosition DLeft (i, j) = (i, j - 1)
updatePosition DRight (i, j) = (i, j + 1)
updatePosition DUp (i, j) = (i - 1, j)
updatePosition DDown (i, j) = (i + 1, j)

