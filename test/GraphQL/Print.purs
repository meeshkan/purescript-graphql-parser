module Test.GraphQL.Print where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, catchError, throwError)
import Data.Either (either)
import Data.Foldable (for_)
import Data.GraphQL.AST (Document(..))
import Data.GraphQL.AST as AST
import Data.GraphQL.AST.Print (printAst)
import Data.GraphQL.Parser as GP
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (Error, throw)
import Parsing (runParser, Parser)
import Test.Data.GraphQL.ParseFull0 (parseDocument)
import Test.Data.GraphQL.ParseFull0 as ParseFull0
import Test.Data.GraphQL.ParseFull1 as ParseFull1
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec ∷ Spec Unit
spec =
  describe "printAst" do

    describe "ParseFull0" do
      checkPrintAndReparse ParseFull0.schema
    describe "ParseFull1" do
      checkPrintAndReparse ParseFull1.query

  where
  checkPrintAndReparse input = do
    it "should parse each definition" do
      (Document defs) <- parseDocument input
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
      doc <- parseDocument input
      let printed = printAst doc
      reparsed <- parseDocument printed
      doc `shouldEqual` reparsed


parseDefinition ∷ String → Aff (AST.Definition)
parseDefinition t = liftEffect (either (throw <<< show) pure (runParser t GP.definition))

-- rountripTest input = do 
--         doc1 <- parseDocument input
--         doc2 <- parseDocument $ printAst doc1
--         doc1 `shouldEqual` doc1

