module Data.FrontMatter.Yaml
  ( FrontMatterContent
  , FrontMatterValue (..)
  , encodeFrontMatter
  , decodeFrontMatter
  ) where

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Array (head, (!!))
import Data.Char (fromCharCode)
import Data.Either (Either(..))
import Data.FrontMatter.JsYaml (decodeYaml, encodeYaml)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.String (null, singleton, trim)
import Data.String.Regex (Regex, match, split, test)
import Data.String.Regex.Flags (multiline, noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Prelude (class Eq, class Show, join, ($), (<$>), (<>))

type FrontMatterContent = String

newtype FrontMatterValue e = FrontMatterValue { attributes :: e, content :: String }

derive instance eqFrontMatterValue :: Eq e => Eq (FrontMatterValue e)

instance showFrontMatterValue :: EncodeJson e => Show (FrontMatterValue e) where
  show (FrontMatterValue { attributes, content }) = encodeFrontMatter attributes content

encodeFrontMatter :: ∀ e. EncodeJson e => e -> FrontMatterContent -> String
encodeFrontMatter obj content =
  sep <> "\n" <> attributes <> sep <> "\n" <>  content
  where
    sep = "---"
    attributes = encodeYaml obj

decodeFrontMatter :: ∀ e. DecodeJson e => String -> Either String (FrontMatterValue e)
decodeFrontMatter fm =
  let firstLine = head $ splitLines fm
  in
    case firstLine of
      Nothing -> Left "invalid text content"
      Just fl | null $ trim fl -> Left "frontmatter value is empty?"
              | testSep fl ->
        case match pattern fm of
          Nothing -> Left $ "invalid format in frontmatter value: \n" <> fm
          Just matches ->
            let get pos = join (matches !! pos)
                sattributes = fromMaybe "" $ get 2
                content = fromMaybe "" $ get 3
                compose attributes = FrontMatterValue { attributes, content }
            in
               compose <$> decodeYaml sattributes
      Just _ -> Left $ "invalid separator? in frontmatter value: " <> fm

pattern :: Regex
pattern = unsafeRegex
            (  "^" <> singleton (fromCharCode 0xFEFF) <> "?" -- optional BOM
            <> "(= yaml =|---)$"                             -- YAML separator
            <> "([\\s\\S]*?)"                                -- attributes
            <> "^(?:\\1|[.]{3})$"                            -- separator again or three dots
            <> "(?:\\r?\\n)?([\\s\\S]*)")                           -- body
            multiline

splitLines :: String -> Array String
splitLines = split $ unsafeRegex "(\r?\n)" noFlags

testSep :: String -> Boolean
testSep = test $ unsafeRegex "= yaml =|---" noFlags
