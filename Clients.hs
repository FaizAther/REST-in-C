module Clients where

import JPoster ( Canvas, things3 )

newtype Iden
    = Iden Int
    deriving (Show, Eq)

data Context
    = MsgN
    | MsgG (String, Int)
    | MsgS (String, Int)
    deriving (Show, Eq)

newtype Client
    = Client (Iden, Canvas, Context)
    deriving (Show, Eq)

data RbTree
    = Empty
    | Node Client RbTree RbTree
    deriving (Show, Eq)

testTree :: RbTree
testTree = Node (Client (Iden 1, things3, MsgN)) Empty Empty