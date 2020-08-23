module PutJson where

-- intercalate xs xss is equivalent to (concat (intersperse xs xss)). 
-- It inserts the list xs in between the lists in xss and concatenates the result.
-- >>> intercalate ", " ["Lorem", "ipsum", "dolor"]
-- "Lorem, ipsum, dolor" 

import Data.List (intercalate)
import SimpleJson

renderJValue :: JValue -> String

renderJValue (JString s) = show s
renderJValue (JNumber n) = show n
renderJValue (JBool True) = "true"
renderJValue (JBool False) = "false"
renderJValue (JNull) = "null"

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
    where pairs [] = ""
          pairs ps = intercalate ", " (map renderPair ps)
          renderPair (k,v) = show k ++ ": " ++ renderJValue v

renderJValue (JArray a) = "[" ++ values a ++ "]"
    where values [] = ""
          values vs = intercalate ", " (map renderJValue vs)

putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)