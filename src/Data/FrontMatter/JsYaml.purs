module Data.FrontMatter.JsYaml where

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, fail)
import Data.Either (Either)
import Prelude (pure, ($), (>>=))

foreign import decodeYamlImpl :: ∀ r. (String -> r) -> (Json -> r) -> String -> r
foreign import encodeYamlImpl :: Json -> String

encodeYaml :: ∀ v. EncodeJson v => v -> String
encodeYaml value = encodeYamlImpl $ encodeJson value

decodeYamlToJson :: String -> Either String Json
decodeYamlToJson yaml = decodeYamlImpl fail pure yaml

decodeYaml :: ∀ v. DecodeJson v => String -> Either String v
decodeYaml s = decodeYamlToJson s >>= decodeJson
