module Instructions where

import Types

data Size = L
    deriving (Eq, Show)

sizeOf :: Type -> Size
sizeOf IntType = L


data Instruction = ADD_I Size Location Location Location
    | MOV_I Size Int Location
    | MOV Size Location Location
    deriving Show