module Debug where


import           Data.Text                     as T
import           Data.Text.Lazy                as TL


notYetImpl :: String -> a
notYetImpl funcName = error $ "[ERROR]<" <> funcName <> "> Not yet implemented"
notYetImplT :: T.Text -> a
notYetImplT funcName =
  error $ "[ERROR]<" <> T.unpack funcName <> "> Not yet implemented"
notYetImplTL :: TL.Text -> a
notYetImplTL funcName =
  error $ "[ERROR]<" <> TL.unpack funcName <> "> Not yet implemented"
