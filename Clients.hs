module Clients where

import JPoster ( Canvas, things3, Jonify (jonify) )

newtype Iden
    = Iden (Int, Stamp)
    deriving (Show, Eq)

data Context
    = MsgN
    | MsgG (String, Int)
    | MsgS (String, Int)
    deriving (Show, Eq)

newtype Stamp
    = Stamp (Int, Int, Int, Int, Int)
    deriving (Show, Eq)

newtype Client
    = Client (Iden, Canvas, Context)
    deriving (Show, Eq)

data RbTree
    = Empty
    | Node Client RbTree RbTree
    deriving (Show, Eq)

manageClient :: RbTree -> Iden -> Maybe Canvas
manageClient Empty _ = Nothing
manageClient (Node (Client (Iden (iV, Stamp sV), canvas, msg)) rC lC) (Iden (iC, Stamp sC))
    | iC == iV = Just canvas
    | otherwise = Just canvas

findClientJ :: RbTree -> Iden -> String
findClientJ t i = let res = manageClient t i in maybe "{ \"BAD\" : \"ID NOT FOUND\" }" jonify res

testTree :: RbTree
testTree = Node (Client (Iden (1, Stamp (0,0,0,0,0)), things3, MsgN)) Empty Empty