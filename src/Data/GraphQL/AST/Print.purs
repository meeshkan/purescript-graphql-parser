module Data.GraphQL.AST.Print where

import Prelude

import Data.Foldable (intercalate)
import Data.GraphQL.AST as AST
import Data.List (List)
import Data.Maybe (Maybe(..), maybe)

--  | Display a GraphQL AST as a graphql string.
class PrintAst a where
  printAst :: a -> String

instance PrintAst AST.Document where
  printAst (AST.Document definitions) =
    intercalate "\n\n" $ map printAst definitions

instance PrintAst AST.Definition where
  printAst = case _ of
    AST.Definition_ExecutableDefinition a -> printAst a
    AST.Definition_TypeSystemDefinition a -> printAst a
    AST.Definition_TypeSystemExtension a -> printAst a

instance PrintAst AST.ExecutableDefinition where
  printAst = case _ of
    AST.ExecutableDefinition_OperationDefinition a -> printAst a
    AST.ExecutableDefinition_FragmentDefinition a -> printAst a

instance PrintAst AST.TypeSystemDefinition where
  printAst = case _ of
    AST.TypeSystemDefinition_SchemaDefinition a -> printAst a
    AST.TypeSystemDefinition_TypeDefinition a -> printAst a
    AST.TypeSystemDefinition_DirectiveDefinition a -> printAst a

instance PrintAst AST.TypeSystemExtension where
  printAst = case _ of
    AST.TypeSystemExtension_SchemaExtension a -> printAst a
    AST.TypeSystemExtension_TypeExtension a -> printAst a

instance PrintAst AST.SchemaExtension where
  printAst = case _ of
    AST.SchemaExtension_With_OperationTypeDefinition
      { directives
      , operationTypesDefinition
      } -> printAst directives
      <> " "
      <> printAst operationTypesDefinition
    AST.SchemaExtension_With_Directives { directives } ->
      printAst directives

instance PrintAst (List AST.OperationTypeDefinition) where
  printAst t = "{\n  " <> intercalate "\n  " (map printAst t) <> "\n}"

instance PrintAst (AST.OperationTypeDefinition) where
  printAst (AST.OperationTypeDefinition { operationType, namedType }) =
    printAst operationType
      <> ": "
      <> printAst namedType

instance PrintAst AST.OperationDefinition where
  printAst = case _ of
    AST.OperationDefinition_SelectionSet a -> printAst a
    AST.OperationDefinition_OperationType { operationType, name, variableDefinitions, directives, selectionSet } ->
      printAst operationType <> " " <> printAst name <> " " <> printAst variableDefinitions <> " " <> printAst directives <> " " <> printAst selectionSet

instance PrintAst AST.SchemaDefinition where
  printAst (AST.SchemaDefinition { directives, rootOperationTypeDefinition }) =
    "schema "
      <> printAst directives
      <> " "
      <>
        ("{\n  " <> intercalate "\n  " (map printAst rootOperationTypeDefinition) <> "\n}")

instance PrintAst AST.RootOperationTypeDefinition where
  printAst (AST.RootOperationTypeDefinition { operationType, namedType }) =
    printAst operationType
      <> ": "
      <> printAst namedType

instance PrintAst AST.TypeDefinition where
  printAst = case _ of
    AST.TypeDefinition_ScalarTypeDefinition a -> printAst a
    AST.TypeDefinition_ObjectTypeDefinition a -> printAst a
    AST.TypeDefinition_InterfaceTypeDefinition a -> printAst a
    AST.TypeDefinition_UnionTypeDefinition a -> printAst a
    AST.TypeDefinition_EnumTypeDefinition a -> printAst a
    AST.TypeDefinition_InputObjectTypeDefinition a -> printAst a

instance PrintAst AST.ScalarTypeDefinition where
  printAst (AST.ScalarTypeDefinition { description, name, directives }) =
    printDescription description
      <> "scalar "
      <> printAst name
      <> " "
      <> printAst directives

instance PrintAst AST.ObjectTypeDefinition where
  printAst (AST.ObjectTypeDefinition { description, name, implementsInterfaces, directives, fieldsDefinition }) =
    printDescription description
      <> "type "
      <> printAst name
      <> " "
      <> printAst implementsInterfaces
      <> printAst directives
      <> printAst fieldsDefinition

instance PrintAst AST.InputObjectTypeDefinition where
  printAst (AST.InputObjectTypeDefinition { description, name, directives, inputFieldsDefinition }) =
    printDescription description
      <> "input "
      <> printAst name
      <> " "
      <> printAst directives
      <> printAst inputFieldsDefinition

instance PrintAst AST.InterfaceTypeDefinition where
  printAst (AST.InterfaceTypeDefinition { description, name, directives, fieldsDefinition }) =
    printDescription description
      <> "interface "
      <> printAst name
      <> " "
      <> printAst directives
      <> printAst fieldsDefinition

instance PrintAst AST.UnionTypeDefinition where
  printAst (AST.UnionTypeDefinition { description, name, directives, unionMemberTypes }) =
    printDescription description
      <> "union "
      <> printAst name
      <> " "
      <> printAst directives
      <> "= "
      <> printAst unionMemberTypes

instance PrintAst AST.EnumTypeDefinition where
  printAst (AST.EnumTypeDefinition { description, name, directives, enumValuesDefinition }) =
    printDescription description
      <> "enum "
      <> printAst name
      <> " "
      <> printAst directives
      <> printAst enumValuesDefinition

instance PrintAst AST.DirectiveDefinition where
  printAst (AST.DirectiveDefinition { description, name, argumentsDefinition, directiveLocations }) =
    printDescription description
      <> "directive @"
      <> printAst name
      <> " "
      <> printAst argumentsDefinition
      <> " on "
      <> printAst directiveLocations

instance PrintAst AST.TypeExtension where
  printAst = case _ of
    AST.TypeExtension_ScalarTypeExtension a -> printAst a
    AST.TypeExtension_ObjectTypeExtension a -> printAst a
    AST.TypeExtension_InterfaceTypeExtension a -> printAst a
    AST.TypeExtension_UnionTypeExtension a -> printAst a
    AST.TypeExtension_EnumTypeExtension a -> printAst a
    AST.TypeExtension_InputObjectTypeExtension a -> printAst a

instance PrintAst AST.ScalarTypeExtension where
  printAst (AST.ScalarTypeExtension t@{ name, directives }) =
    "scalar "
      <> printAst name
      <> " "
      <> printAst directives

printExtension :: String -> String
printExtension s = "extend " <> s <> " "

instance PrintAst AST.InputObjectTypeExtension where
  printAst t = printExtension "input" <> case t of
    AST.InputObjectTypeExtension_With_Directives { directives, name } ->
      printAst name
        <> " "
        <> printAst directives
    AST.InputObjectTypeExtension_With_InputFieldsDefinition { directives, name, inputFieldsDefinition } ->
      printAst name
        <> " "
        <> printAst directives
        <> printAst inputFieldsDefinition

instance PrintAst AST.EnumTypeExtension where
  printAst t = printExtension "enum" <> case t of
    AST.EnumTypeExtension_With_Directives { directives, name } ->
      printAst name
        <> " "
        <> printAst directives
    AST.EnumTypeExtension_With_EnumValuesDefinition { directives, name, enumValuesDefinition } ->
      printAst name
        <> " "
        <> printAst directives
        <> printAst enumValuesDefinition

instance PrintAst AST.InterfaceTypeExtension where
  printAst t = printExtension "input" <> case t of
    AST.InterfaceTypeExtension_With_FieldsDefinition { directives, fieldsDefinition, name } ->
      printAst name
        <> " "
        <> printAst directives
        <> printAst fieldsDefinition
    AST.InterfaceTypeExtension_With_Directives { directives, name } ->
      printAst name
        <> " "
        <> printAst directives

instance PrintAst AST.UnionTypeExtension where
  printAst t = printExtension "union" <> case t of
    AST.UnionTypeExtension_With_Directives { directives, name } ->
      printAst name
        <> " "
        <> printAst directives
    AST.UnionTypeExtension_With_UnionMemberTypes { directives, name, unionMemberTypes } ->
      printAst name
        <> " "
        <> printAst directives
        <> "= "
        <> printAst unionMemberTypes

instance PrintAst AST.ObjectTypeExtension where
  printAst t = printExtension "type" <> case t of
    AST.ObjectTypeExtension_With_ImplementsInterfaces { name, implementsInterfaces } ->
      printAst name
        <> " "
        <> printAst implementsInterfaces
    AST.ObjectTypeExtension_With_Directives { name, implementsInterfaces, directives } ->
      printAst name
        <> " "
        <> printAst implementsInterfaces
        <> printAst directives
    AST.ObjectTypeExtension_With_FieldsDefinition { name, implementsInterfaces, directives, fieldsDefinition } ->
      printAst name
        <> " "
        <> printAst implementsInterfaces
        <> printAst directives
        <> printAst fieldsDefinition

instance PrintAst AST.ImplementsInterfaces where
  printAst (AST.ImplementsInterfaces fields) =
    "implements " <> intercalate " & " (map printAst fields)

instance PrintAst AST.InputFieldsDefinition where
  printAst (AST.InputFieldsDefinition fields) =
    "{\n  " <> intercalate "\n  " (map printAst fields) <> "\n}"

instance PrintAst AST.EnumValuesDefinition where
  printAst (AST.EnumValuesDefinition fields) =
    "{\n  " <> intercalate "\n  " (map printAst fields) <> "\n}"

instance PrintAst AST.DirectiveLocations where
  printAst (AST.DirectiveLocations fields) =
    intercalate " | " (map printAst fields)

instance PrintAst AST.UnionMemberTypes where
  printAst (AST.UnionMemberTypes fields) =
    intercalate " | " (map printAst fields)

instance PrintAst AST.DirectiveLocation where
  printAst = case _ of
    AST.DirectiveLocation_TypeSystemDirectiveLocation a -> printAst a
    AST.DirectiveLocation_ExecutableDirectiveLocation a -> printAst a

instance PrintAst AST.TypeSystemDirectiveLocation where
  printAst = show

instance PrintAst AST.ExecutableDirectiveLocation where
  printAst = show

instance PrintAst AST.EnumValueDefinition where
  printAst (AST.EnumValueDefinition { description, enumValue, directives }) =
    printDescription description
      <> printAst enumValue
      <> printAst directives

instance PrintAst AST.EnumValue where
  printAst (AST.EnumValue name) = name

instance PrintAst AST.FieldsDefinition where
  printAst (AST.FieldsDefinition fields) =
    "{\n  " <> intercalate "\n  " (map printAst fields) <> "\n}"

instance PrintAst AST.FieldDefinition where
  printAst (AST.FieldDefinition t@{ description, name, argumentsDefinition, directives }) =
    printDescription description
      <> " "
      <> printAst name
      <> printAst argumentsDefinition
      <> ": "
      <> printAst t.type
      <> printAst directives

instance PrintAst AST.ArgumentsDefinition where
  printAst (AST.ArgumentsDefinition args) =
    "(" <> intercalate ", " (map printAst args) <> ")"

instance PrintAst AST.InputValueDefinition where
  printAst (AST.InputValueDefinition t@{ description, name, defaultValue, directives }) =
    printDescription description
      <> printAst name
      <> ": "
      <> printAst t.type
      <> printAst directives

instance PrintAst AST.OperationType where
  printAst = case _ of
    AST.Query -> "query"
    AST.Mutation -> "mutation"
    AST.Subscription -> "subscription"

instance PrintAst AST.FragmentDefinition where
  printAst (AST.FragmentDefinition f) =
    "fragment " <> printAst f.typeCondition

instance PrintAst AST.TypeCondition where
  printAst (AST.TypeCondition t) =
    "on " <> printAst t

instance PrintAst AST.SelectionSet where
  printAst (AST.SelectionSet t) =
    "{\n  " <> intercalate "\n  " (map printAst t) <> "\n}\n"

instance PrintAst AST.Selection where
  printAst = case _ of
    AST.Selection_Field a -> printAst a
    AST.Selection_FragmentSpread a -> printAst a
    AST.Selection_InlineFragment a -> printAst a

instance PrintAst AST.Field where
  printAst (AST.Field { alias, name, arguments, directives, selectionSet }) =
    printAlias alias
      <> " "
      <> printAst name
      <> " "
      <> printAst arguments
      <> " "
      <> printAst directives
      <> " "
      <> printAst selectionSet

    where
    printAlias = case _ of
      Just s -> s <> ": "
      Nothing -> ""

instance PrintAst AST.FragmentSpread where
  printAst (AST.FragmentSpread { fragmentName, directives }) =
    "..."
      <> printAst fragmentName
      <> printAst directives

instance PrintAst AST.InlineFragment where
  printAst (AST.InlineFragment { typeCondition, directives, selectionSet }) =
    "..."
      <> printAst typeCondition
      <> printAst directives
      <> printAst selectionSet

instance PrintAst AST.VariableDefinitions where
  printAst (AST.VariableDefinitions t) =
    "(" <> intercalate " " (map printAst t) <> ")"

instance PrintAst AST.Directives where
  printAst (AST.Directives t) =
    intercalate " " $ map printAst t

instance PrintAst AST.Directive where
  printAst (AST.Directive { name, arguments }) =
    "@" <> name <> printAst arguments

instance PrintAst AST.Arguments where
  printAst (AST.Arguments t) =
    "(" <> intercalate ", " (map printAst t) <> ")"

instance PrintAst AST.Argument where
  printAst (AST.Argument { name, value }) =
    name <> ": " <> printAst value

instance PrintAst AST.VariableDefinition where
  printAst (AST.VariableDefinition t@{ variable, defaultValue }) =
    printAst variable <> ": " <> printAst t.type <> printAst defaultValue

instance PrintAst AST.DefaultValue where
  printAst a = "=" <> printAst a

-- VariableDefinition

-- types 

instance PrintAst AST.Type where
  printAst = case _ of
    AST.Type_NamedType t -> printAst t
    AST.Type_ListType t -> printAst t
    AST.Type_NonNullType t -> printAst t

instance PrintAst AST.ListType where
  printAst (AST.ListType t) = "[" <> printAst t <> "]"

instance PrintAst AST.NonNullType where
  printAst t_ =
    ( case t_ of
        (AST.NonNullType_NamedType t) -> printAst t
        (AST.NonNullType_ListType t) -> printAst t
    ) <> "! "

-- values 

instance PrintAst AST.Value where
  printAst = case _ of
    AST.Value_Variable a -> printAst a
    AST.Value_IntValue a -> printAst a
    AST.Value_FloatValue a -> printAst a
    AST.Value_StringValue a -> printAst a
    AST.Value_BooleanValue a -> printAst a
    AST.Value_NullValue a -> printAst a
    AST.Value_EnumValue a -> printAst a
    AST.Value_ListValue a -> printAst a
    AST.Value_ObjectValue a -> printAst a

instance PrintAst AST.Variable where
  printAst (AST.Variable t) = "$" <> printAst t

instance PrintAst AST.IntValue where
  printAst (AST.IntValue t) = show t

instance PrintAst AST.FloatValue where
  printAst (AST.FloatValue t) = show t

instance PrintAst AST.StringValue where
  printAst (AST.StringValue t) = show t

instance PrintAst AST.BooleanValue where
  printAst (AST.BooleanValue t) = show t

instance PrintAst AST.ListValue where
  printAst (AST.ListValue t) = "[" <> (intercalate " " $ map printAst t) <> "]"

instance PrintAst AST.ObjectValue where
  printAst (AST.ObjectValue t) = "{ " <> (intercalate " " $ map printAst t) <> " }"

instance PrintAst AST.NullValue where
  printAst AST.NullValue = "null"

-- names 

instance PrintAst AST.NamedType where
  printAst (AST.NamedType a) = a

-- maybes

instance PrintAst a => PrintAst (Maybe a) where
  printAst = maybe "" printAst

instance PrintAst String where
  printAst a = a

-- utils 

printDescription :: Maybe String -> String
printDescription = maybe "" tripleQuote

tripleQuote :: String -> String
tripleQuote s = q3 <> s <> q3
  where
  q3 = "\"\"\""
