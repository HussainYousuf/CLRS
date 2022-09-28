module SimpleJson where

data JValue
    = JString {getString :: String}
    | JNumber {getNumber :: Double}
    | JBool {getBool :: Bool}
    | JNull
    | JArray {getArray :: [JValue]}
    | JObject {getObject :: [(String, JValue)]}

type Doc = String
