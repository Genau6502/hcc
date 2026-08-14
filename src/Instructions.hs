module Instructions where

import Types

data Size = L
    deriving (Eq, Show)

sizeOf :: Type -> Size
sizeOf IntType = L


data Instruction = ADD Size Location Location Location
    | SUB Size Location Location Location
    | IMUL Size Location Location Location
    | MOV_I Size Int Location
    | MOV Size Location Location
    deriving Eq

instance Show Instruction where
    show (MOV_I s i l) = "mov" ++ show s ++ " $" ++ show i ++ ", " ++ show l
    show (MOV s i l) = "mov" ++ show s ++ " " ++ show i ++ ", " ++ show l
    show (ADD s l1 l2 l3) = "add" ++ show s ++ " " ++ show l1 ++ ", " ++ show l2 ++ ", " ++ show l3
    show (SUB s l1 l2 l3) = "sub" ++ show s ++ " " ++ show l1 ++ ", " ++ show l2 ++ ", " ++ show l3
    show (IMUL s l1 l2 l3) = "imul" ++ show s ++ " " ++ show l1 ++ ", " ++ show l2 ++ ", " ++ show l3