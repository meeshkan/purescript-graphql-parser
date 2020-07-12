module Test.Data.GraphQL.ParseFull3 where

import Prelude
import Data.Either (either)
import Data.GraphQL.AST as AST
import Data.GraphQL.Parser as GP
import Data.Lens (class Wander)
import Data.Lens as L
import Data.List (length)
import Data.Profunctor.Choice (class Choice)
import Data.Tuple (uncurry)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Test.Spec (SpecT, before, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.Parser (runParser)

parseDocument :: String -> Aff AST.Document
parseDocument t = do
  rtf <- liftEffect $ readTextFile UTF8 t
  liftEffect (either (throw <<< show) pure (runParser rtf GP.document))

lensToObjectDefinitions ∷ ∀ m. Choice m ⇒ Wander m ⇒ m AST.ObjectTypeDefinition AST.ObjectTypeDefinition → m AST.Document AST.Document
lensToObjectDefinitions =
  ( uncurry L.prism' AST._Document
      <<< L.traversed
      <<< uncurry L.prism' AST._Definition_TypeSystemDefinition
      <<< uncurry L.prism' AST._TypeSystemDefinition_TypeDefinition
      <<< uncurry L.prism' AST._TypeDefinition_ObjectTypeDefinition
  )

testSwapi ∷ ∀ m. Monad m ⇒ SpecT Aff Unit m Unit
testSwapi =
  describe "test swapi" do
    before (parseDocument "schemas/swapi.graphql")
      $ do
          -- 52 was confirmed by a quick n' dirty parsing of the document in python
          it "should get 52 type definitions" \doc → do
            (length (L.toListOf lensToObjectDefinitions doc)) `shouldEqual` 52
