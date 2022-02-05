module Communication where

import Foreign.C ( CString, newCString )
import JPoster (things3, Jonify (jonify))
import Clients (manageClient, testTree, findClientJ, Iden (Iden), Stamp (Stamp))

canvasStr :: IO CString
canvasStr = newCString $ findClientJ testTree $ Iden (1, Stamp (0,0,0,0,0))

foreign export ccall canvasStr :: IO CString
