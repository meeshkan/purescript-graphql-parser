module Test.GraphQL.Print where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
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
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec ∷ Spec Unit
spec =
  describe "printAst" do

    -- describe "simple" do 
    --   it "should print simple" do
    --     parseSuccess parseDocument "simple" "simple"

    
    describe "ParseFull0" do 
      it "should parse the full schema" do
        (Document defs) <- parseDocument ParseFull0.schema

        for_ defs \def -> do 
          logShow def
          let printed = printAst def

          log printed

          defReprinted <- parseDefinition printed

          when (def /= defReprinted) $ do 
            log "definition print failed"
            log printed

          def `shouldEqual` defReprinted
        pure unit

parseDefinition ∷ String → Aff (AST.Definition)
parseDefinition t = liftEffect (either (throw <<< show) pure (runParser t GP.definition))


-- rountripTest input = do 
--         doc1 <- parseDocument input
--         doc2 <- parseDocument $ printAst doc1
--         doc1 `shouldEqual` doc1

