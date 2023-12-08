module Parser where

import Prelude hiding (take, getLine)
import qualified Data.Attoparsec.Text as P
import Data.Attoparsec.Text (Parser)
import Types

getLine :: Parser Hand
getLine = do
    s <- P.take 5
    d <- P.skipSpace >> P.decimal
    pure $ Hand (Label s) d
