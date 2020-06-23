module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Data.GraphQL.ParseFull0 (testFullDoc)
import Test.Data.GraphQL.ParseFull1 (testQuery)
import Test.Data.GraphQL.ParseSimple (testParser)
import Test.Data.GraphQL.ParseSadistic0 (testSadistic0)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main ∷ Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        testParser
        testFullDoc
        testQuery
        testSadistic0
