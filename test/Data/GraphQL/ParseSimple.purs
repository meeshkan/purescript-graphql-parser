module Test.Data.GraphQL.ParseSimple where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (either)
import Data.GraphQL.AST as AST
import Data.GraphQL.Parser as GP
import Data.List (List(..), (:), singleton)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Effect.Exception (Error)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Text.Parsing.Parser (runParser, Parser)
import Text.Parsing.Parser.String (class StringLike)

parseSuccess ∷ ∀ s t m. StringLike s ⇒ MonadThrow Error m ⇒ Show t ⇒ Eq t ⇒ Parser s t → s → t → m Unit
parseSuccess parser toparse tocomp = either (fail <<< show) (shouldEqual tocomp) (runParser toparse parser)

testParser ∷ forall e m. Monad m => Bind e => MonadThrow Error e => SpecT e Unit m Unit
testParser =
  describe "test parser" do
    describe "test tokens" do
      it "should correctly parse comments" do
        parseSuccess GP.comment "# abc" unit
        parseSuccess GP.comment "#" unit
      it "should correctly parse variables" do
        parseSuccess GP.variable "$_x" (AST.Variable "_x")
      it "should correctly parse integers" do
        parseSuccess GP.intValue "42" (AST.IntValue 42)
        parseSuccess GP.intValue "-42" (AST.IntValue (negate 42))
      it "should correctly parse numbers" do
        parseSuccess GP.floatValue "3.1416" (AST.FloatValue 3.1416)
        parseSuccess GP.floatValue "-0.1416e+3" (AST.FloatValue $ negate 0.1416e+3)
      it "should correctly parse booleans" do
        parseSuccess GP.booleanValue "true" (AST.BooleanValue true)
        parseSuccess GP.booleanValue "false" (AST.BooleanValue false)
      it "should correctly parse null" do
        parseSuccess GP.nullValue "null" (AST.NullValue)
      it "should correctly parse strings" do
        parseSuccess GP.stringValue (fromCharArray [ '"', 'a', 'b', '\\', 'n', 'q', '"' ]) (AST.StringValue "ab\nq")
        parseSuccess GP.stringValue (fromCharArray [ '"', 'a', 'b', '\\', 'u', '0', '0', '2', '1', 'q', '"' ]) (AST.StringValue "ab!q")
        parseSuccess GP.stringValue (fromCharArray [ '"', '"', '"', 'a', 'b', '\\', 'n', 'q', '"', '"', '"' ]) (AST.StringValue """ab\nq""")
      it "should correctly parse lists" do
        parseSuccess (GP.listValue GP.value) "[]" (AST.ListValue (Nil))
        parseSuccess (GP.listValue GP.value) "[1]" (AST.ListValue (AST.Value_IntValue (AST.IntValue 1) : Nil))
        parseSuccess (GP.listValue GP.value) "[\n\n#hello\n\n]" (AST.ListValue (Nil))
        parseSuccess (GP.listValue GP.value) "[\t\t1 2  \t,,  \"3\" ]" (AST.ListValue (AST.Value_IntValue (AST.IntValue 1) : AST.Value_IntValue (AST.IntValue 2) : AST.Value_StringValue (AST.StringValue "3") : Nil))
        parseSuccess (GP.listValue GP.value) "[1 2 \"3\"]" (AST.ListValue (AST.Value_IntValue (AST.IntValue 1) : AST.Value_IntValue (AST.IntValue 2) : AST.Value_StringValue (AST.StringValue "3") : Nil))
      it "should correctly parse objects" do
        parseSuccess (GP.objectValue GP.value) "{}" (AST.ObjectValue (Nil))
        parseSuccess (GP.objectValue GP.value) "{foo: 1}" (AST.ObjectValue (AST.Argument { name: "foo", value: (AST.Value_IntValue $ AST.IntValue 1) } : Nil))
        parseSuccess (GP.objectValue GP.value) "{foo: $bar}" (AST.ObjectValue (AST.Argument { name: "foo", value: (AST.Value_Variable $ AST.Variable "bar") } : Nil))
        parseSuccess (GP.objectValue GP.value) "{foo: BAR}" (AST.ObjectValue (AST.Argument { name: "foo", value: (AST.Value_EnumValue $ AST.EnumValue "BAR") } : Nil))
        parseSuccess (GP.objectValue GP.value) "{foo: BAR, baz: \"hello\"}" (AST.ObjectValue (AST.Argument { name: "foo", value: (AST.Value_EnumValue $ AST.EnumValue "BAR") } : AST.Argument { name: "baz", value: (AST.Value_StringValue $ AST.StringValue "hello") } : Nil))
        parseSuccess (GP.objectValue GP.value) "{foo: BAR baz: \"hello\"      \t}" (AST.ObjectValue (AST.Argument { name: "foo", value: (AST.Value_EnumValue $ AST.EnumValue "BAR") } : AST.Argument { name: "baz", value: (AST.Value_StringValue $ AST.StringValue "hello") } : Nil))
    describe "test field" do
      it "should correctly parse simple field" do
        parseSuccess (GP.field GP.selectionSet) "foo" (AST.Field { alias: Nothing, name: "foo", arguments: Nothing, directives: Nothing, selectionSet: Nothing })
        parseSuccess (GP.field GP.selectionSet) "bar: foo" (AST.Field { alias: Just "bar", name: "foo", arguments: Nothing, directives: Nothing, selectionSet: Nothing })
        parseSuccess (GP.field GP.selectionSet) "bar:foo" (AST.Field { alias: Just "bar", name: "foo", arguments: Nothing, directives: Nothing, selectionSet: Nothing })
        parseSuccess (GP.field GP.selectionSet) "bar\t#\n:foo" (AST.Field { alias: Just "bar", name: "foo", arguments: Nothing, directives: Nothing, selectionSet: Nothing })
    describe "test selectionSet" do
      it "should correctly parse selectionSet" do
        parseSuccess GP.selectionSet "{ \n#sel\nfoo }" (AST.SelectionSet (AST.Selection_Field (AST.Field { alias: Nothing, name: "foo", arguments: Nothing, directives: Nothing, selectionSet: Nothing }) : Nil))
        parseSuccess (GP.selection GP.selectionSet) "foo" (AST.Selection_Field (AST.Field { alias: Nothing, name: "foo", arguments: Nothing, directives: Nothing, selectionSet: Nothing }))
        parseSuccess GP.selectionSet "{foo}" (AST.SelectionSet (AST.Selection_Field (AST.Field { alias: Nothing, name: "foo", arguments: Nothing, directives: Nothing, selectionSet: Nothing }) : Nil))
        parseSuccess GP.selectionSet "{ foo { foo {foo}#hello\n} }" (AST.SelectionSet (AST.Selection_Field (AST.Field { alias: Nothing, name: "foo", arguments: Nothing, directives: Nothing, selectionSet: Just (AST.SelectionSet (AST.Selection_Field (AST.Field { alias: Nothing, name: "foo", arguments: Nothing, directives: Nothing, selectionSet: Just (AST.SelectionSet (AST.Selection_Field (AST.Field { alias: Nothing, name: "foo", arguments: Nothing, directives: Nothing, selectionSet: Nothing }) : Nil)) }) : Nil)) }) : Nil))
        parseSuccess GP.selectionSet "{ foo { foo {foo},,,#hello\n} }" (AST.SelectionSet (AST.Selection_Field (AST.Field { alias: Nothing, name: "foo", arguments: Nothing, directives: Nothing, selectionSet: Just (AST.SelectionSet (AST.Selection_Field (AST.Field { alias: Nothing, name: "foo", arguments: Nothing, directives: Nothing, selectionSet: Just (AST.SelectionSet (AST.Selection_Field (AST.Field { alias: Nothing, name: "foo", arguments: Nothing, directives: Nothing, selectionSet: Nothing }) : Nil)) }) : Nil)) }) : Nil))
        parseSuccess GP.selectionSet "{foo { foo { foo } } }" (AST.SelectionSet (AST.Selection_Field (AST.Field { alias: Nothing, name: "foo", arguments: Nothing, directives: Nothing, selectionSet: Just (AST.SelectionSet (AST.Selection_Field (AST.Field { alias: Nothing, name: "foo", arguments: Nothing, directives: Nothing, selectionSet: Just (AST.SelectionSet (AST.Selection_Field (AST.Field { alias: Nothing, name: "foo", arguments: Nothing, directives: Nothing, selectionSet: Nothing }) : Nil)) }) : Nil)) }) : Nil))
    describe "test fieldDefinition" do
      it "should parse a field definition" do
        parseSuccess GP.fieldDefinition "id: ID!" (AST.FieldDefinition { description: Nothing, name: "id", argumentsDefinition: Nothing, type: (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType "ID"))), directives: Nothing })
    describe "test variable definition" do
      it "should parse a variable definition" do
        parseSuccess GP.variableDefinition "$id:ID!" (AST.VariableDefinition { variable: AST.Variable "id", type: AST.Type_NonNullType (AST.NonNullType_NamedType $ AST.NamedType "ID"), defaultValue: Nothing })
    describe "test parse an alias correclty" do
      it "should parse a simple alias correctly" do
        parseSuccess GP.alias "z3  :" "z3"
        parseSuccess (GP.field GP.selectionSet) "z9: Z" $ AST.Field { alias: Just "z9", name: "Z", arguments: Nothing, directives: Nothing, selectionSet: Nothing }
        parseSuccess (GP.selection GP.selectionSet) "z9: Z" (AST.Selection_Field (AST.Field { alias: Just "z9", name: "Z", arguments: Nothing, directives: Nothing, selectionSet: Nothing }))
        parseSuccess GP.selectionSet "{ z9: Z }" $ AST.SelectionSet (singleton (AST.Selection_Field (AST.Field { alias: Just "z9", name: "Z", arguments: Nothing, directives: Nothing, selectionSet: Nothing })))
    describe "should parse selection set correctly" do
      it "should parse a selection set with multiple values" do
        parseSuccess GP.selectionSet "{ id user }" $ AST.SelectionSet ((AST.Selection_Field (AST.Field { alias: Nothing, name: "id", arguments: Nothing, directives: Nothing, selectionSet: Nothing })) : (AST.Selection_Field (AST.Field { alias: Nothing, name: "user", arguments: Nothing, directives: Nothing, selectionSet: Nothing })) : Nil)
