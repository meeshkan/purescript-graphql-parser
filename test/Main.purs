module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Data.GraphQL.ParseFull0 (testFullDoc)
import Test.Data.GraphQL.ParseFull1 (testQuery)
import Test.Data.GraphQL.ParseFull2 (testCS)
import Test.Data.GraphQL.ParseSadistic0 (testSadistic0)
import Test.Data.GraphQL.ParseSadistic1 (testSadistic1)
import Test.Data.GraphQL.ParseSimple (testParser)
import Test.Data.GraphQL.RetrieveStringTypes (retrieveStringTypes)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main ∷ Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        testParser
        testFullDoc
        testQuery
        testCS
        retrieveStringTypes
        testSadistic0
        testSadistic1
