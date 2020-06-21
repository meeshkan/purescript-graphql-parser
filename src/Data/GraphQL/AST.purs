module Data.GraphQL.AST where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.Maybe (Maybe)

derive instance documentGeneric ∷ Generic Document _

instance documentShow ∷ Show Document where
  show v = genericShow v

derive instance documentEq ∷ Eq Document

data Document
  = Document (List Definition)

derive instance definitionGeneric ∷ Generic Definition _

instance definitionShow ∷ Show Definition where
  show v = genericShow v

derive instance definitionEq ∷ Eq Definition

data Definition
  = Definition_ExecutableDefinition ExecutableDefinition
  | Definition_TypeSystemDefinition TypeSystemDefinition
  | Definition_TypeSystemExtension TypeSystemExtension

derive instance executableDefinitionGeneric ∷ Generic ExecutableDefinition _

instance executableDefinitionShow ∷ Show ExecutableDefinition where
  show v = genericShow v

derive instance executableDefinitionEq ∷ Eq ExecutableDefinition

data ExecutableDefinition
  = ExecutableDefinition_OperationDefinition OperationDefinition
  | ExecutableDefinition_FragmentDefinition FragmentDefinition

derive instance operationDefinitionGeneric ∷ Generic OperationDefinition _

instance operationDefinitionShow ∷ Show OperationDefinition where
  show v = genericShow v

derive instance operationDefinitionEq ∷ Eq OperationDefinition

data OperationDefinition
  = OperationDefinition_SelectionSet SelectionSet
  | OperationDefinition_OperationType { operationType ∷ OperationType, name ∷ (Maybe String), variableDefinitions ∷ (Maybe VariableDefinitions), directives ∷ (Maybe Directives), selectionSet ∷ SelectionSet }

derive instance operationTypeGeneric ∷ Generic OperationType _

instance operationTypeShow ∷ Show OperationType where
  show v = genericShow v

derive instance operationTypeEq ∷ Eq OperationType

data OperationType
  = Query
  | Mutation
  | Subscription

derive instance selectionSetGeneric ∷ Generic SelectionSet _

instance selectionSetShow ∷ Show SelectionSet where
  show v = genericShow v

derive instance selectionSetEq ∷ Eq SelectionSet

data SelectionSet
  = SelectionSet (List Selection)

derive instance selectionGeneric ∷ Generic Selection _

instance selectionShow ∷ Show Selection where
  show v = genericShow v

derive instance selectionEq ∷ Eq Selection

data Selection
  = Selection_Field Field
  | Selection_FragmentSpread FragmentSpread
  | Selection_InlineFragment InlineFragment

derive instance fieldGeneric ∷ Generic Field _

instance fieldShow ∷ Show Field where
  show v = genericShow v

derive instance fieldEq ∷ Eq Field

data Field
  = Field { alias ∷ (Maybe String), name ∷ String, arguments ∷ (Maybe Arguments), directives ∷ (Maybe Directives), selectionSet ∷ (Maybe SelectionSet) }

derive instance argumentsGeneric ∷ Generic Arguments _

instance argumentsShow ∷ Show Arguments where
  show v = genericShow v

derive instance argumentsEq ∷ Eq Arguments

data Arguments
  = Arguments (List Argument)

derive instance argumentGeneric ∷ Generic Argument _

instance argumentShow ∷ Show Argument where
  show v = genericShow v

derive instance argumentEq ∷ Eq Argument

data Argument
  = Argument String Value

derive instance fragmentSpreadGeneric ∷ Generic FragmentSpread _

instance fragmentSpreadShow ∷ Show FragmentSpread where
  show v = genericShow v

derive instance fragmentSpreadEq ∷ Eq FragmentSpread

data FragmentSpread
  = FragmentSpread { fragmentName ∷ String, directives ∷ Maybe Directives }

derive instance inlineFragmentGeneric ∷ Generic InlineFragment _

instance inlineFragmentShow ∷ Show InlineFragment where
  show v = genericShow v

derive instance inlineFragmentEq ∷ Eq InlineFragment

data InlineFragment
  = InlineFragment { typeCondition ∷ (Maybe TypeCondition), directives ∷ (Maybe Directives), selectionSet ∷ SelectionSet }

derive instance fragmentDefinitionGeneric ∷ Generic FragmentDefinition _

instance fragmentDefinitionShow ∷ Show FragmentDefinition where
  show v = genericShow v

derive instance fragmentDefinitionEq ∷ Eq FragmentDefinition

data FragmentDefinition
  = FragmentDefinition { fragmentName ∷ String, typeCondition ∷ TypeCondition, directives ∷ (Maybe Directives), selectionSet ∷ SelectionSet }

derive instance typeConditionGeneric ∷ Generic TypeCondition _

instance typeConditionShow ∷ Show TypeCondition where
  show v = genericShow v

derive instance typeConditionEq ∷ Eq TypeCondition

data TypeCondition
  = TypeCondition NamedType

derive instance valueGeneric ∷ Generic Value _

instance valueShow ∷ Show Value where
  show v = genericShow v

derive instance valueEq ∷ Eq Value

data Value
  = Value_Variable Variable
  | Value_IntValue IntValue
  | Value_FloatValue FloatValue
  | Value_StringValue StringValue
  | Value_BooleanValue BooleanValue
  | Value_NullValue NullValue
  | Value_EnumValue EnumValue
  | Value_ListValue ListValue
  | Value_ObjectValue ObjectValue

derive instance intValueGeneric ∷ Generic IntValue _

instance intValueShow ∷ Show IntValue where
  show v = genericShow v

derive instance intValueEq ∷ Eq IntValue

data IntValue
  = IntValue Int

derive instance floatValueGeneric ∷ Generic FloatValue _

instance floatValueShow ∷ Show FloatValue where
  show v = genericShow v

derive instance floatValueEq ∷ Eq FloatValue

data FloatValue
  = FloatValue Number

derive instance booleanValueGeneric ∷ Generic BooleanValue _

instance booleanValueShow ∷ Show BooleanValue where
  show v = genericShow v

derive instance booleanValueEq ∷ Eq BooleanValue

data BooleanValue
  = BooleanValue Boolean

derive instance stringValueGeneric ∷ Generic StringValue _

instance stringValueShow ∷ Show StringValue where
  show v = genericShow v

derive instance stringValueEq ∷ Eq StringValue

data StringValue
  = StringValue String

derive instance nullValueGeneric ∷ Generic NullValue _

instance nullValueShow ∷ Show NullValue where
  show v = genericShow v

derive instance nullValueEq ∷ Eq NullValue

data NullValue
  = NullValue

derive instance enumValueGeneric ∷ Generic EnumValue _

instance enumValueShow ∷ Show EnumValue where
  show v = genericShow v

derive instance enumValueEq ∷ Eq EnumValue

data EnumValue
  = EnumValue String

derive instance listValueGeneric ∷ Generic ListValue _

instance listValueShow ∷ Show ListValue where
  show v = genericShow v

derive instance listValueEq ∷ Eq ListValue

data ListValue
  = ListValue (List Value)

derive instance objectValueGeneric ∷ Generic ObjectValue _

instance objectValueShow ∷ Show ObjectValue where
  show v = genericShow v

derive instance objectValueEq ∷ Eq ObjectValue

data ObjectValue
  = ObjectValue (List Argument)

derive instance variableDefinitionsGeneric ∷ Generic VariableDefinitions _

instance variableDefinitionsShow ∷ Show VariableDefinitions where
  show v = genericShow v

derive instance variableDefinitionsEq ∷ Eq VariableDefinitions

data VariableDefinitions
  = VariableDefinitions (List VariableDefinition)

derive instance variableDefinitionGeneric ∷ Generic VariableDefinition _

instance variableDefinitionShow ∷ Show VariableDefinition where
  show v = genericShow v

derive instance variableDefinitionEq ∷ Eq VariableDefinition

data VariableDefinition
  = VariableDefinition { variable ∷ Variable, type ∷ Type, defaultValue ∷ (Maybe DefaultValue) }

derive instance variableGeneric ∷ Generic Variable _

instance variableShow ∷ Show Variable where
  show v = genericShow v

derive instance variableEq ∷ Eq Variable

data Variable
  = Variable String

derive instance defaultValueGeneric ∷ Generic DefaultValue _

instance defaultValueShow ∷ Show DefaultValue where
  show v = genericShow v

derive instance defaultValueEq ∷ Eq DefaultValue

data DefaultValue
  = DefaultValue Value

derive instance typeGeneric ∷ Generic Type _

instance typeShow ∷ Show Type where
  show v = genericShow v

derive instance typeEq ∷ Eq Type

data Type
  = Type_NamedType NamedType
  | Type_ListType ListType
  | Type_NonNullType NonNullType

derive instance namedTypeGeneric ∷ Generic NamedType _

instance namedTypeShow ∷ Show NamedType where
  show v = genericShow v

derive instance namedTypeEq ∷ Eq NamedType

data NamedType
  = NamedType String

derive instance listTypeGeneric ∷ Generic ListType _

instance listTypeShow ∷ Show ListType where
  show v = genericShow v

derive instance listTypeEq ∷ Eq ListType

data ListType
  = ListType (List Type)

derive instance nonNullTypeGeneric ∷ Generic NonNullType _

instance nonNullTypeShow ∷ Show NonNullType where
  show v = genericShow v

derive instance nonNullTypeEq ∷ Eq NonNullType

data NonNullType
  = NonNullType_NamedType NamedType
  | NonNullType_ListType ListType

derive instance directivesGeneric ∷ Generic Directives _

instance directivesShow ∷ Show Directives where
  show v = genericShow v

derive instance directivesEq ∷ Eq Directives

data Directives
  = Directives (List Directive)

derive instance directiveGeneric ∷ Generic Directive _

instance directiveShow ∷ Show Directive where
  show v = genericShow v

derive instance directiveEq ∷ Eq Directive

data Directive
  = Directive { name ∷ String, arguments ∷ (Maybe Arguments) }

derive instance typeSystemDefinitionGeneric ∷ Generic TypeSystemDefinition _

instance typeSystemDefinitionShow ∷ Show TypeSystemDefinition where
  show v = genericShow v

derive instance typeSystemDefinitionEq ∷ Eq TypeSystemDefinition

data TypeSystemDefinition
  = TypeSystemDefinition_SchemaDefinition SchemaDefinition
  | TypeSystemDefinition_TypeDefinition TypeDefinition
  | TypeSystemDefinition_DirectiveDefinition DirectiveDefinition

derive instance typeSystemExtensionGeneric ∷ Generic TypeSystemExtension _

instance typeSystemExtensionShow ∷ Show TypeSystemExtension where
  show v = genericShow v

derive instance typeSystemExtensionEq ∷ Eq TypeSystemExtension

data TypeSystemExtension
  = TypeSystemExtension_SchemaExtension SchemaExtension
  | TypeSystemExtension_TypeExtension TypeExtension

derive instance schemaDefinitionGeneric ∷ Generic SchemaDefinition _

instance schemaDefinitionShow ∷ Show SchemaDefinition where
  show v = genericShow v

derive instance schemaDefinitionEq ∷ Eq SchemaDefinition

data SchemaDefinition
  = SchemaDefinition { directives ∷ (Maybe Directives), rootOperationTypeDefinition ∷ (List RootOperationTypeDefinition) }

derive instance rootOperationTypeDefinitionGeneric ∷ Generic RootOperationTypeDefinition _

instance rootOperationTypeDefinitionShow ∷ Show RootOperationTypeDefinition where
  show v = genericShow v

derive instance rootOperationTypeDefinitionEq ∷ Eq RootOperationTypeDefinition

data RootOperationTypeDefinition
  = RootOperationTypeDefinition OperationType NamedType

derive instance schemaExtensionGeneric ∷ Generic SchemaExtension _

instance schemaExtensionShow ∷ Show SchemaExtension where
  show v = genericShow v

derive instance schemaExtensionEq ∷ Eq SchemaExtension

data SchemaExtension
  = SchemaExtension_With_OperationTypeDefinition { directives ∷ (Maybe Directives), operationTypesDefinition ∷ (List OperationTypeDefinition) }
  | SchemaExtension_With_Directives { directives ∷ Directives }

derive instance operationTypeDefinitionGeneric ∷ Generic OperationTypeDefinition _

instance operationTypeDefinitionShow ∷ Show OperationTypeDefinition where
  show v = genericShow v

derive instance operationTypeDefinitionEq ∷ Eq OperationTypeDefinition

data OperationTypeDefinition
  = OperationTypeDefinition OperationType NamedType

derive instance typeDefinitionGeneric ∷ Generic TypeDefinition _

instance typeDefinitionShow ∷ Show TypeDefinition where
  show v = genericShow v

derive instance typeDefinitionEq ∷ Eq TypeDefinition

data TypeDefinition
  = TypeDefinition_ScalarTypeDefinition ScalarTypeDefinition
  | TypeDefinition_ObjectTypeDefinition ObjectTypeDefinition
  | TypeDefinition_InterfaceTypeDefinition InterfaceTypeDefinition
  | TypeDefinition_UnionTypeDefinition UnionTypeDefinition
  | TypeDefinition_EnumTypeDefinition EnumTypeDefinition
  | TypeDefinition_InputObjectTypeDefinition InputObjectTypeDefinition

derive instance typeExtensionGeneric ∷ Generic TypeExtension _

instance typeExtensionShow ∷ Show TypeExtension where
  show v = genericShow v

derive instance typeExtensionEq ∷ Eq TypeExtension

data TypeExtension
  = TypeExtension_ScalarTypeExtension ScalarTypeExtension
  | TypeExtension_ObjectTypeExtension ObjectTypeExtension
  | TypeExtension_InterfaceTypeExtension InterfaceTypeExtension
  | TypeExtension_UnionTypeExtension UnionTypeExtension
  | TypeExtension_EnumTypeExtension EnumTypeExtension
  | TypeExtension_InputObjectTypeExtension InputObjectTypeExtension

derive instance scalarTypeDefinitionGeneric ∷ Generic ScalarTypeDefinition _

instance scalarTypeDefinitionShow ∷ Show ScalarTypeDefinition where
  show v = genericShow v

derive instance scalarTypeDefinitionEq ∷ Eq ScalarTypeDefinition

data ScalarTypeDefinition
  = ScalarTypeDefinition { description ∷ (Maybe String), name ∷ String, directives ∷ (Maybe Directives) }

derive instance scalarTypeExtensionGeneric ∷ Generic ScalarTypeExtension _

instance scalarTypeExtensionShow ∷ Show ScalarTypeExtension where
  show v = genericShow v

derive instance scalarTypeExtensionEq ∷ Eq ScalarTypeExtension

data ScalarTypeExtension
  = ScalarTypeExtension { name ∷ String, directives ∷ Directives }

derive instance objectTypeDefinitionGeneric ∷ Generic ObjectTypeDefinition _

instance objectTypeDefinitionShow ∷ Show ObjectTypeDefinition where
  show v = genericShow v

derive instance objectTypeDefinitionEq ∷ Eq ObjectTypeDefinition

data ObjectTypeDefinition
  = ObjectTypeDefinition { description ∷ (Maybe String), name ∷ String, implementsInterfaces ∷ (Maybe ImplementsInterfaces), directives ∷ (Maybe Directives), fieldsDefinition ∷ (Maybe FieldsDefinition) }

derive instance objectTypeExtensionGeneric ∷ Generic ObjectTypeExtension _

instance objectTypeExtensionShow ∷ Show ObjectTypeExtension where
  show v = genericShow v

derive instance objectTypeExtensionEq ∷ Eq ObjectTypeExtension

data ObjectTypeExtension
  = ObjectTypeExtension_With_FieldsDefinition { name ∷ String, implementsInterfaces ∷ (Maybe ImplementsInterfaces), directives ∷ (Maybe Directives), fieldsDefinition ∷ FieldsDefinition }
  | ObjectTypeExtension_With_Directives { name ∷ String, implementsInterfaces ∷ (Maybe ImplementsInterfaces), directives ∷ Directives }
  | ObjectTypeExtension_With_ImplementsInterfaces { name ∷ String, implementsInterfaces ∷ ImplementsInterfaces }

derive instance implementsInterfacesGeneric ∷ Generic ImplementsInterfaces _

instance implementsInterfacesShow ∷ Show ImplementsInterfaces where
  show v = genericShow v

derive instance implementsInterfacesEq ∷ Eq ImplementsInterfaces

data ImplementsInterfaces
  = ImplementsInterfaces (List NamedType)

derive instance fieldsDefinitionGeneric ∷ Generic FieldsDefinition _

instance fieldsDefinitionShow ∷ Show FieldsDefinition where
  show v = genericShow v

derive instance fieldsDefinitionEq ∷ Eq FieldsDefinition

data FieldsDefinition
  = FieldsDefinition (List FieldDefinition)

derive instance fieldDefinitionGeneric ∷ Generic FieldDefinition _

instance fieldDefinitionShow ∷ Show FieldDefinition where
  show v = genericShow v

derive instance fieldDefinitionEq ∷ Eq FieldDefinition

data FieldDefinition
  = FieldDefinition { description ∷ (Maybe String), name ∷ String, argumentsDefinition ∷ (Maybe ArgumentsDefinition), type ∷ Type, directives ∷ (Maybe Directives) }

derive instance argumentsDefinitionGeneric ∷ Generic ArgumentsDefinition _

instance argumentsDefinitionShow ∷ Show ArgumentsDefinition where
  show v = genericShow v

derive instance argumentsDefinitionEq ∷ Eq ArgumentsDefinition

data ArgumentsDefinition
  = ArgumentsDefinition (List InputValueDefinition)

derive instance inputValueDefinitionGeneric ∷ Generic InputValueDefinition _

instance inputValueDefinitionShow ∷ Show InputValueDefinition where
  show v = genericShow v

derive instance inputValueDefinitionEq ∷ Eq InputValueDefinition

data InputValueDefinition
  = InputValueDefinition { description ∷ (Maybe String), name ∷ String, type ∷ Type, defaultValue ∷ (Maybe DefaultValue), directives ∷ (Maybe Directives) }

derive instance interfaceTypeDefinitionGeneric ∷ Generic InterfaceTypeDefinition _

instance interfaceTypeDefinitionShow ∷ Show InterfaceTypeDefinition where
  show v = genericShow v

derive instance interfaceTypeDefinitionEq ∷ Eq InterfaceTypeDefinition

data InterfaceTypeDefinition
  = InterfaceTypeDefinition { description ∷ (Maybe String), name ∷ String, directives ∷ (Maybe Directives), fieldsDefinition ∷ (Maybe FieldsDefinition) }

derive instance interfaceTypeExtensionGeneric ∷ Generic InterfaceTypeExtension _

instance interfaceTypeExtensionShow ∷ Show InterfaceTypeExtension where
  show v = genericShow v

derive instance interfaceTypeExtensionEq ∷ Eq InterfaceTypeExtension

data InterfaceTypeExtension
  = InterfaceTypeExtension_With_FieldsDefinition { name ∷ String, directives ∷ (Maybe Directives), fieldsDefinition ∷ FieldsDefinition }
  | InterfaceTypeExtension_With_Directives { name ∷ String, directives ∷ Directives }

derive instance unionTypeDefinitionGeneric ∷ Generic UnionTypeDefinition _

instance unionTypeDefinitionShow ∷ Show UnionTypeDefinition where
  show v = genericShow v

derive instance unionTypeDefinitionEq ∷ Eq UnionTypeDefinition

data UnionTypeDefinition
  = UnionTypeDefinition { description ∷ Maybe String, name ∷ String, directives ∷ (Maybe Directives), unionMemberTypes ∷ (Maybe UnionMemberTypes) }

derive instance unionMemberTypesGeneric ∷ Generic UnionMemberTypes _

instance unionMemberTypesShow ∷ Show UnionMemberTypes where
  show v = genericShow v

derive instance unionMemberTypesEq ∷ Eq UnionMemberTypes

data UnionMemberTypes
  = UnionMemberTypes (List NamedType)

derive instance unionTypeExtensionGeneric ∷ Generic UnionTypeExtension _

instance unionTypeExtensionShow ∷ Show UnionTypeExtension where
  show v = genericShow v

derive instance unionTypeExtensionEq ∷ Eq UnionTypeExtension

data UnionTypeExtension
  = UnionTypeExtension_With_UnionMemberTypes { name ∷ String, directives ∷ (Maybe Directives), unionMemberTypes ∷ UnionMemberTypes }
  | UnionTypeExtension_With_Directives { name ∷ String, directives ∷ Directives }

derive instance enumTypeDefinitionGeneric ∷ Generic EnumTypeDefinition _

instance enumTypeDefinitionShow ∷ Show EnumTypeDefinition where
  show v = genericShow v

derive instance enumTypeDefinitionEq ∷ Eq EnumTypeDefinition

data EnumTypeDefinition
  = EnumTypeDefinition { description ∷ (Maybe String), name ∷ String, directives ∷ (Maybe Directives), enumValuesDefinition ∷ (Maybe EnumValuesDefinition) }

derive instance enumValuesDefinitionGeneric ∷ Generic EnumValuesDefinition _

instance enumValuesDefinitionShow ∷ Show EnumValuesDefinition where
  show v = genericShow v

derive instance enumValuesDefinitionEq ∷ Eq EnumValuesDefinition

data EnumValuesDefinition
  = EnumValuesDefinition (List EnumValueDefinition)

derive instance enumValueDefinitionGeneric ∷ Generic EnumValueDefinition _

instance enumValueDefinitionShow ∷ Show EnumValueDefinition where
  show v = genericShow v

derive instance enumValueDefinitionEq ∷ Eq EnumValueDefinition

data EnumValueDefinition
  = EnumValueDefinition { description ∷ (Maybe String), enumValue ∷ EnumValue, directives ∷ (Maybe Directives) }

derive instance enumTypeExtensionGeneric ∷ Generic EnumTypeExtension _

instance enumTypeExtensionShow ∷ Show EnumTypeExtension where
  show v = genericShow v

derive instance enumTypeExtensionEq ∷ Eq EnumTypeExtension

data EnumTypeExtension
  = EnumTypeExtension_With_EnumValuesDefinition { name ∷ String, directives ∷ (Maybe Directives), enumValuesDefinition ∷ EnumValuesDefinition }
  | EnumTypeExtension_With_Directives { name ∷ String, directives ∷ Directives }

derive instance inputObjectTypeDefinitionGeneric ∷ Generic InputObjectTypeDefinition _

instance inputObjectTypeDefinitionShow ∷ Show InputObjectTypeDefinition where
  show v = genericShow v

derive instance inputObjectTypeDefinitionEq ∷ Eq InputObjectTypeDefinition

data InputObjectTypeDefinition
  = InputObjectTypeDefinition { description ∷ (Maybe String), name ∷ String, directives ∷ (Maybe Directives), inputFieldsDefinition ∷ (Maybe InputFieldsDefinition) }

derive instance inputFieldsDefinitionGeneric ∷ Generic InputFieldsDefinition _

instance inputFieldsDefinitionShow ∷ Show InputFieldsDefinition where
  show v = genericShow v

derive instance inputFieldsDefinitionEq ∷ Eq InputFieldsDefinition

data InputFieldsDefinition
  = InputFieldsDefinition (List InputValueDefinition)

derive instance inputObjectTypeExtensionGeneric ∷ Generic InputObjectTypeExtension _

instance inputObjectTypeExtensionShow ∷ Show InputObjectTypeExtension where
  show v = genericShow v

derive instance inputObjectTypeExtensionEq ∷ Eq InputObjectTypeExtension

data InputObjectTypeExtension
  = InputObjectTypeExtension_With_InputFieldsDefinition { name ∷ String, directives ∷ (Maybe Directives), inputFieldsDefinition ∷ InputFieldsDefinition }
  | InputObjectTypeExtension_With_Directives { name ∷ String, directives ∷ Directives }

derive instance directiveDefinitionGeneric ∷ Generic DirectiveDefinition _

instance directiveDefinitionShow ∷ Show DirectiveDefinition where
  show v = genericShow v

derive instance directiveDefinitionEq ∷ Eq DirectiveDefinition

data DirectiveDefinition
  = DirectiveDefinition { description ∷ (Maybe String), name ∷ String, argumentsDefinition ∷ (Maybe ArgumentsDefinition), directiveLocations ∷ DirectiveLocations }

derive instance directiveLocationsGeneric ∷ Generic DirectiveLocations _

instance directiveLocationsShow ∷ Show DirectiveLocations where
  show v = genericShow v

derive instance directiveLocationsEq ∷ Eq DirectiveLocations

data DirectiveLocations
  = DirectiveLocations (List DirectiveLocation)

derive instance directiveLocationGeneric ∷ Generic DirectiveLocation _

instance directiveLocationShow ∷ Show DirectiveLocation where
  show v = genericShow v

derive instance directiveLocationEq ∷ Eq DirectiveLocation

data DirectiveLocation
  = DirectiveLocation_ExecutableDirectiveLocation ExecutableDirectiveLocation
  | DirectiveLocation_TypeSystemDirectiveLocation TypeSystemDirectiveLocation

derive instance executableDirectiveLocationGeneric ∷ Generic ExecutableDirectiveLocation _

instance executableDirectiveLocationShow ∷ Show ExecutableDirectiveLocation where
  show v = genericShow v

derive instance executableDirectiveLocationEq ∷ Eq ExecutableDirectiveLocation

data ExecutableDirectiveLocation
  = QUERY
  | MUTATION
  | SUBSCRIPTION
  | FIELD
  | FRAGMENT_DEFINITION
  | FRAGMENT_SPREAD
  | INLINE_FRAGMENT

derive instance typeSystemDirectiveLocationGeneric ∷ Generic TypeSystemDirectiveLocation _

instance typeSystemDirectiveLocationShow ∷ Show TypeSystemDirectiveLocation where
  show v = genericShow v

derive instance typeSystemDirectiveLocationEq ∷ Eq TypeSystemDirectiveLocation

data TypeSystemDirectiveLocation
  = SCHEMA
  | SCALAR
  | OBJECT
  | FIELD_DEFINITION
  | ARGUMENT_DEFINITION
  | INTERFACE
  | UNION
  | ENUM
  | ENUM_VALUE
  | INPUT_OBJECT
  | INPUT_FIELD_DEFINITION
