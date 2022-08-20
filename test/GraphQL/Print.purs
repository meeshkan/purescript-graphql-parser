module Test.GraphQL.Print where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Data.Either (either)
import Data.Foldable (for_)
import Data.GraphQL.AST (Document(..))
import Data.GraphQL.AST as AST
import Data.GraphQL.AST.Print (printAst)
import Data.GraphQL.Parser as GP
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Parsing (runParser)
import Test.Data.GraphQL.ParseFull0 (parseDocument)
import Test.Data.GraphQL.ParseFull0 as ParseFull0
import Test.Data.GraphQL.ParseFull1 as ParseFull1
import Test.Data.GraphQL.ParseFull2 as ParseFull2
import Test.Data.GraphQL.ParseFull3 as ParseFull3
import Test.Data.GraphQL.ParseSadistic0 as ParseSadistic0
import Test.Data.GraphQL.ParseSadistic1 as ParseSadistic1
import Test.Data.GraphQL.RetrieveStringTypes as RetrieveStringTypes

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec =
  describe "printAst" do

    describe "ParseFull0" do
      checkPrintAndReparse ParseFull0.schema
    describe "ParseFull1" do
      checkPrintAndReparse ParseFull1.query
    describe "ParseFull2" do
      checkPrintAndReparse ParseFull2.query
    describe "swapi" do
      checkPrintAndReparse_ $ ParseFull3.parseDocument "schemas/swapi.graphql"
    describe "ParseSadistic0" do
      checkPrintAndReparse ParseSadistic0.query
    describe "ParseSadistic1" do
      checkPrintAndReparse ParseSadistic1.query
    describe "RetrieveStringTypes" do
      checkPrintAndReparse RetrieveStringTypes.query
  where
  checkPrintAndReparse schema = checkPrintAndReparse_ (parseDocument schema)
  checkPrintAndReparse_ getInput = do
    it "should parse each definition" do
      (Document defs) <- getInput
      for_ defs \def -> do
        let printed = printAst def
        catchError
          ( do
              defReprinted <- parseDefinition printed
              def `shouldEqual` defReprinted
          )
          \err -> do
            log $ "Failed at: "
            log printed
            throwError err

    it "should parse the full schema" do
      doc <- getInput
      let printed = printAst doc
      reparsed <- parseDocument printed
      doc `shouldEqual` reparsed

parseDefinition ∷ String → Aff (AST.Definition)
parseDefinition t = liftEffect (either (throw <<< show) pure (runParser t GP.definition))

-- rountripTest input = do 
--         doc1 <- parseDocument input
--         doc2 <- parseDocument $ printAst doc1
--         doc1 `shouldEqual` doc1

