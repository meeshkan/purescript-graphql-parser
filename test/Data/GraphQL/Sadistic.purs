module Test.Data.GraphQL.Sadistic where

import Prelude
import Data.Either (either)
import Data.GraphQL.AST as AST
import Data.GraphQL.Parser as GP
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.String (class StringLike)

parseDocument ∷ ∀ s. StringLike s ⇒ s → Aff (AST.Document)
parseDocument t = liftEffect (either (throw <<< show) pure (runParser t GP.document))

query =
  """subscription
Z999999999       @Z9999999 ( Z9999 : "Z99999" ) @Z9  @Z999999 ( Z999 : null
Z9_9999 :
""      )    @Z9999999999 ( Z999999   : 117041 Z :      "՜棔籱" Z9999999 : "Z9999999" )
@Z999
( Z9999999 : [ true 0.11807158827691878 ] )      @Z999999 (      Z999999_999 :
0.5673602519404889     Z99999999
: $Z99999999    Z99999999 : { Z9999999 : 0.06845007001815832 } )      @Z9999 
@Z
( Z9999 : { Z9999999          : "Z"   Z999999 : { Z99999 : null Z999      :    [ -747954
null ] } Z9999999 :     0.5752561439644807  } Z99 :          [   { Z9 :
$Z99 Z99999 : $Z Z999999 :      false Z9 : "霣齺鳓"          Z999 :    null }  null
false "Z9999" "Z" ]         Z99999 :    "Z99999" ) @Z999999  @Z999999999 ( Z9999999 :
null Z9999999 : { Z9999999      : -813435 Z99_999 : "죙ꖪ붬㴊" Z9 : { Z999
: [ 0.003608481028866247 ] Z9 : null Z99999 : 0.034653820579244675          Z9999 :
"Z9" Z9 : "繌⤪얹"        Z :        [           ] } } Z99999         : 515488 )
 { ... on Z999        {         ...           {      ... Z  @Z9 ( Z9999 :
"Z99" ) @Z9     ( Z          : "Z" Z     :          "㶸뢇" Z999 : "Z9" )
 ... on Z9999  @Z999 (      Z999 : null     Z9999 : {     Z9 : 275792 }
Z9     : [ {   }    ] ) @Z999 ( Z9999 : 0.30698200515796525 ) @Z9 (          Z99
: "ထ꨸"       Z999 : "Z99"         Z99 :        false ) @Z ( Z999 :       {     Z99
: $Z } Z9       : null Z9_99 : $Z99 )  {           }         ... on Z9999
 @Z999 (          Z9999 :   "Z" Z9 : { Z9_ : true } Z99 : { Z9 : {  } }         Z
: 0.4402643346415201 ) @Z999 (  ) @Z (  ) @Z99 ( Z9 : [  ] Z9999 : $Z9 ) 
{  }   ...   @Z9    ( Z9 : "Z" Z9999 : $Z9 Z : false ) @Z9  ( Z9 : 0.017671687071058755 Z
: false       Z999 : "Z" Z99 : null )  {          } } ... Z999  @Z9999         ( Z999
: -832414 Z999     :        $Z9999   Z999999 : [ 0.9526554122346711 null       "Z9" -768155
] ) @Z99999  @Z99 ( _9        :      0.28971920734723994 Z99999 : true Z : null Z999
: false Z99999 : -479226 Z99 : true ) @Z    (  ) @Z ( Z9 : -256546 Z9        : true Z99         :      null
Z99999 : null Z999999 : "Z9999" Z9999 : 0.42235427322907104 ) @Z         ...          Z999999
 @Z9999 ( Z :        {  }        Z99999 : null Z_99 :    0.11202365770564585 Z999999 :
null         Z99999 : "Z9999" ) @Z999 (  ) @Z99_9 (  ) @Z999  @Z999999  @Z99     ( Z999__
: 0.06906225535509282 Z9 : "" Z9 : null Z999999 : "Z" Z99 : "Z99" )       Z9 :      Z999 (
Z9 : null Z9999 :  null Z999999 :    [ [ [  ] ]       true      null ] )  { ... on Z999       @Z9999
( Z : { Z99 : false } Z   : 0.2429428665167386 Z9999 : "Z9" ) @Z9999 ( Z9999 : true Z99
: "꧁쀳" Z9999 : null ) @Z         (  Z9 : "ು벡" Z9999 : { Z :       true Z : true }
Z : $Z99 Z : { Z9 : 0.43586298331425666     Z9 : {  } } )  { ...   {  } Z : Z99      (
 )  @Z9 ( Z99 : false      Z99          : "Z" )  {  } } ...        Z9999  }
... on Z999999  @Z9 ( Z999999         : { Z9999 : { Z99         : -121627 } }       Z : $Z99_
Z9999 :          { Z : -850968 Z : true } Z : $Z999     Z9 : null       Z999999   :         null )      @Z999
(  )     {  }        Z999999 :          Z99_             { ...   on Z  @Z   { ... Z  @Z9 (  )      }
... Z99       } }        }""" ∷
    String

testSadistic ∷ ∀ m. Monad m ⇒ SpecT Aff Unit m Unit
testSadistic =
  describe "test sadistic query query" do
    it "should exist" do
      either (\s -> fail $ "Could not parse: " <> show s) (\_ -> 1 `shouldEqual` 1) $ runParser query GP.operationDefinition
