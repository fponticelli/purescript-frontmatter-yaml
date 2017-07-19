module Test.Main where

import Data.FrontMatter.Yaml
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Char (fromCharCode)
import Data.Either (Either(..))
import Data.FrontMatter.JsYaml (decodeYaml, encodeYaml)
import Data.Newtype (class Newtype, unwrap)
import Data.String (singleton)
import Prelude (class Eq, Unit, bind, discard, pure, ($), (+), (<>), (==))
import Test.Unit (Test, failure, suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: forall e. Eff ( console :: CONSOLE , testOutput :: TESTOUTPUT , avar :: AVAR | e) Unit
main = runTest do
  suite "basic assumptions about json/yaml" do
    test "roundtrip json" do
      assert "serializing+deserializing to json should produce the same object"
        $ (decodeJson $ encodeJson instTitleAndDescription) == (Right instTitleAndDescription)
    test "roundtrip yaml" do
      assert "serializing+deserializing to yaml should produce the same object"
        $ (decodeYaml $ encodeYaml instTitleAndDescription) == (Right instTitleAndDescription)
    test "sample" do
      assert "2 + 2 should be 4" $ (2 + 2) == 4
  suite "encoding" do
    test "encoding manually works" do
      let encoded = encodeFrontMatter instTitleAndDescription "text"
      assert ("\"" <> basic <> "\"\nshould be the same as\n\"" <> encoded <> "\"") $  basic == encoded
  suite "decoding" do
    test "decoding basic manually works" do
      let decoded = decodeFrontMatter basic
      assert "decoding basic string should work" $  decoded == (Right (FrontMatterValue { attributes: instTitleAndDescription, content: "text" }))
    test "decoding string with bom" do
      let decoded = decodeFrontMatter bom
      assert "decoding string with bom should work" $  decoded == (Right (FrontMatterValue { attributes: instTitle, content: "" }))
  suite "roundtrip frontmatter" do
    test "roundtrip basic" do
      assertRoundtripFrontMatter instTitleAndDescription "text"
  suite "roundtrip from encoded" do
    test "roundtrip dotsEnding" do
      testDotsEnding
    test "roundtrip yamlSeparator" do
      testYamlSeparator

assertRoundtripFrontMatter :: ∀ te. TitleAndDescription -> String -> Test te
assertRoundtripFrontMatter inst text =
  let uinst = unwrap inst
      encoded = encodeFrontMatter inst text
      decoded = decodeFrontMatter encoded
  in
    assertDecoded { title: uinst.title, description: uinst.description, content: text } decoded

assertDecoded :: ∀ e. { title :: String, description :: String, content :: String } -> (Either String (FrontMatterValue TitleAndDescription)) -> Test e
assertDecoded exp t =
  case t of
    Right (FrontMatterValue { attributes: TitleAndDescription {title, description} , content }) -> do
      equal exp.title title
      equal exp.description description
      equal exp.content content
    Left e ->
      failure $ "unable to decode: " <> e

testDotsEnding :: ∀ e. Test e
testDotsEnding =
  let decoded = decodeFrontMatter dotsEnding :: Either String (FrontMatterValue TitleAndDescription)
  in
    assertDecoded
      { title: "Example with dots document ending"
      , description: "Just an example of using `...`"
      , content: "\nIt shouldn't break with ..." } decoded

testYamlSeparator :: ∀ e. Test e
testYamlSeparator =
  let decoded = decodeFrontMatter yamlSeparator :: Either String (FrontMatterValue TitleAndDescription)
  in
    assertDecoded
      { title: "I couldn't think of a better name"
      , description: "Just an example of using `= yaml =`"
      , content: "\nPlays nice with markdown syntax highlighting" } decoded

-- TitleAndDescription

newtype TitleAndDescription = TitleAndDescription { title :: String, description :: String }

instance encodeTitleAndDescription :: EncodeJson TitleAndDescription where
  encodeJson (TitleAndDescription td)
    = "title" := td.title
   ~> "description" := td.description
   ~> jsonEmptyObject

instance decodeTitleAndDescription :: DecodeJson TitleAndDescription where
  decodeJson json = do
    obj <- decodeJson json
    title <- obj .? "title"
    description <- obj .? "description"
    pure $ TitleAndDescription { title, description }

derive instance eqTitleAndDescription :: Eq TitleAndDescription
derive instance newtypeTitleAndDescription :: Newtype TitleAndDescription _

instTitleAndDescription :: TitleAndDescription
instTitleAndDescription = TitleAndDescription { title: "A Story", description: "A story with a description." }

-- Title

newtype Title = Title { title :: String }

instance encodeTitle :: EncodeJson Title where
  encodeJson (Title td)
    = "title" := td.title
   ~> jsonEmptyObject

instance decodeTitle :: DecodeJson Title where
  decodeJson json = do
    obj <- decodeJson json
    title <- obj .? "title"
    pure $ Title { title }

derive instance eqTitle :: Eq Title

instTitle :: Title
instTitle = Title { title: "Relax guy, I'm not hiding any BOMs" }

-- TEST CASES

basic :: String
basic = """---
description: A story with a description.
title: A Story
---
text"""

bom :: String
bom = singleton (fromCharCode 0xFEFF) <> """---
title: Relax guy, I'm not hiding any BOMs
---
"""

complexYaml :: String
complexYaml = """---

title: This is a title!

name: Derek Worthen
age: young
contact:
  email: email@domain.com
  address: some location
pets:
- cat
- dog
- bat
match: !!js/regexp /pattern/gim



---

- item
- item
- item"""

dashesSeparator :: String
dashesSeparator = """---
title: Three dashes marks the spot
tags:
  - yaml
  - front-matter
  - dashes
expaned-description: with some --- crazy stuff in it
---

don't break

---

Also this shouldn't be a problem"""

dotsEnding :: String
dotsEnding = """---
title: Example with dots document ending
description: Just an example of using `...`
...

It shouldn't break with ..."""

missingBody :: String
missingBody = """---
title: Three dashes marks the spot
tags:
  - yaml
  - front-matter
  - dashes
expaned-description: with some --- crazy stuff in it
---
"""

noFrontMatter :: String
noFrontMatter = """# Hello World

This is a markdown document.

---

Some text here that is definitely not front matter.

---

More text here."""

wrappedText :: String
wrappedText = """---
title: Complex yaml example
description: You can use the front-matter module to convert this
tags: [example, yaml, node]
folded-text: |
  There once was a man from Darjeeling
  Who got on a bus bound for Ealing
      It said on the door
      "Please don't spit on the floor"
  So he carefully spat on the ceiling
wrapped-text: >
  Wrapped text
  will be folded
  into a single
  paragraph

  Blank lines denote
  paragraph breaks
---

Some crazy stuff going on up there ^^"""

yamlSeparator :: String
yamlSeparator = """= yaml =
title: I couldn't think of a better name
description: Just an example of using `= yaml =`
= yaml =

Plays nice with markdown syntax highlighting"""
