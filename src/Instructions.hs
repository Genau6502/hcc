module Instructions where

import Types

data Size = L
    deriving (Eq, Show)

sizeOf :: Type -> Size
sizeOf IntType = L


data Instruction = ADD_I Size Location Location Location
    | MOV_I Size Int Location
    | MOV Size Location Location
    deriving Eq

instance Show Instruction where
    show (MOV_I s i l) = "mov" ++ show s ++ " $" ++ show i ++ ", " ++ show l