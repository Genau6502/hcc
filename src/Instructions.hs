module Instructions where

import Types

data Size = L
    deriving Eq

instance Show Size where
    show L = "l"

sizeOf :: Type -> Size
sizeOf IntType = L


data Instruction = Label Int
    | ADD Size Location Location Location
    | SUB Size Location Location Location
    | IMUL Size Location Location Location
    | MOV Size Location Location
    | CMP Size Location Location
    | TEST Size Location Location
    | JE Instruction
    | JMP Instruction
    deriving Eq

instance Show Instruction where
    show (Label i) = ".L" ++ show i
    show (MOV s i l) = "mov" ++ show s ++ " " ++ show i ++ ", " ++ show l
    show (ADD s l1 l2 l3) = "add" ++ show s ++ " " ++ show l1 ++ ", " ++ show l2 ++ ", " ++ show l3
    show (SUB s l1 l2 l3) = "sub" ++ show s ++ " " ++ show l1 ++ ", " ++ show l2 ++ ", " ++ show l3
    show (IMUL s l1 l2 l3) = "imul" ++ show s ++ " " ++ show l1 ++ ", " ++ show l2 ++ ", " ++ show l3
    show (CMP s l1 l2) = "cmp" ++ show s ++ " " ++ show l1 ++ ", " ++ show l2
    show (TEST s l1 l2) = "test" ++ show s ++ " " ++ show l1 ++ ", " ++ show l2
    show (JE (Label i)) = "je L" ++ show i
    show (JMP (Label i)) = "jmp L" ++ show i
    show _ = "Error: illegal instruction"
