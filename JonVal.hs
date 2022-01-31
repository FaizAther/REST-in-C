module JonVal where

import Data.Char (toLower)

data JonVal
  = JonNul
  | JonBool Bool
  | JonNum Int
  | JonLit String
  | JonList [JonVal]
  | JonMap [(String, JonVal)]
  deriving (Eq)

showJonMapElm :: (String, JonVal) -> String
showJonMapElm (key, value) = show key ++ ":" ++ show value

instance Show JonVal where
  show JonNul = "null"
  show (JonBool val) = map toLower $ show val
  show (JonNum val) = show val
  show (JonLit val) = show val
  show (JonList vals) = show vals
  show (JonMap pairs) = "{" ++ concat alternat ++ "}"
    where
      fix comB elm = (if comB then "," else "") ++ showJonMapElm elm
      alternat = map (fix False) (take 1 pairs) ++ map (fix True) (drop 1 pairs)
