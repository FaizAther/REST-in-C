{-# LANGUAGE ForeignFunctionInterface #-}

module Thing where

import Foreign.C ( CString, newCString )

newtype Url = Url String
            deriving (Show, Eq)

data Content    = Nothing
                | Words String
                | Picture Url
                | Video Url
                deriving (Show, Eq)

data Duration   = Done
                | Seconds Int Duration
                | Minutes Int Duration
                | Hours Int Duration
                | Days Int Duration
                deriving (Show, Eq)

newtype Height  = Height Int
                deriving (Show, Eq)

newtype Width   = Width Int
                deriving (Show, Eq)


newtype Position    = Position (Height, Width)
                    deriving (Show, Eq)


newtype Element = Element (Position, Duration, Content)
                deriving (Show, Eq)


data Canvas     = One [Element]
                | Many [Canvas] 
                deriving (Show, Eq)

canvasStr :: IO CString
canvasStr = (newCString . show) things

foreign export ccall canvasStr :: IO CString

toCanvas :: [String] -> Canvas
toCanvas xs = One (map (\s -> Element (Position (Height 0, Width 0), Done, Words s)) xs)

things :: Canvas
things = toCanvas ["Hello", "This", "Is", "A", "Test"]
