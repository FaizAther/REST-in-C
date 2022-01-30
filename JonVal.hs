module JonVal where

data JonVal
  = JonNul
  | JonBool Bool
  | JonNum Int
  | JonLit String
  | JonList [JonVal]
  | JonMap [(String, JonVal)]
  deriving (Show, Eq)