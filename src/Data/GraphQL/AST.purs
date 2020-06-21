module Data.GraphQL.AST where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.Maybe (Maybe(..))

derive instance documentGeneric ∷ Generic Document _

instance documentShow ∷ Show Document where
  show v = genericShow v

derive instance documentEq ∷ Eq Document

_Document ∷
  Tuple
    ( (List Definition) → Document
    )
    ( Document →
      Maybe (List Definition)
    )
_Document =
  Tuple Document
    ( case _ of
        Document a → Just a
    )

data Document
  = Document (List Definition)

derive instance definitionGeneric ∷ Generic Definition _

instance definitionShow ∷ Show Definition where
  show v = genericShow v

derive instance definitionEq ∷ Eq Definition

_Definition_ExecutableDefinition ∷
  Tuple
    ( ExecutableDefinition → Definition
    )
    ( Definition →
      Maybe ExecutableDefinition
    )
_Definition_ExecutableDefinition =
  Tuple Definition_ExecutableDefinition
    ( case _ of
        Definition_ExecutableDefinition a → Just a
        _ → Nothing
    )

_Definition_TypeSystemDefinition ∷
  Tuple
    ( TypeSystemDefinition → Definition
    )
    ( Definition →
      Maybe TypeSystemDefinition
    )
_Definition_TypeSystemDefinition =
  Tuple Definition_TypeSystemDefinition
    ( case _ of
        Definition_TypeSystemDefinition a → Just a
        _ → Nothing
    )

_Definition_TypeSystemExtension ∷
  Tuple
    ( TypeSystemExtension → Definition
    )
    ( Definition →
      Maybe TypeSystemExtension
    )
_Definition_TypeSystemExtension =
  Tuple Definition_TypeSystemExtension
    ( case _ of
        Definition_TypeSystemExtension a → Just a
        _ → Nothing
    )

data Definition
  = Definition_ExecutableDefinition ExecutableDefinition
  | Definition_TypeSystemDefinition TypeSystemDefinition
  | Definition_TypeSystemExtension TypeSystemExtension

derive instance executableDefinitionGeneric ∷ Generic ExecutableDefinition _

instance executableDefinitionShow ∷ Show ExecutableDefinition where
  show v = genericShow v

derive instance executableDefinitionEq ∷ Eq ExecutableDefinition

_ExecutableDefinition_OperationDefinition ∷
  Tuple
    ( OperationDefinition → ExecutableDefinition
    )
    ( ExecutableDefinition →
      Maybe OperationDefinition
    )
_ExecutableDefinition_OperationDefinition =
  Tuple ExecutableDefinition_OperationDefinition
    ( case _ of
        ExecutableDefinition_OperationDefinition a → Just a
        _ → Nothing
    )

_ExecutableDefinition_FragmentDefinition ∷
  Tuple
    ( FragmentDefinition → ExecutableDefinition
    )
    ( ExecutableDefinition →
      Maybe FragmentDefinition
    )
_ExecutableDefinition_FragmentDefinition =
  Tuple ExecutableDefinition_FragmentDefinition
    ( case _ of
        ExecutableDefinition_FragmentDefinition a → Just a
        _ → Nothing
    )

data ExecutableDefinition
  = ExecutableDefinition_OperationDefinition OperationDefinition
  | ExecutableDefinition_FragmentDefinition FragmentDefinition

derive instance operationDefinitionGeneric ∷ Generic OperationDefinition _

instance operationDefinitionShow ∷ Show OperationDefinition where
  show v = genericShow v

derive instance operationDefinitionEq ∷ Eq OperationDefinition

_OperationDefinition_SelectionSet ∷
  Tuple
    ( SelectionSet → OperationDefinition
    )
    ( OperationDefinition →
      Maybe SelectionSet
    )
_OperationDefinition_SelectionSet =
  Tuple OperationDefinition_SelectionSet
    ( case _ of
        OperationDefinition_SelectionSet a → Just a
        _ → Nothing
    )

_OperationDefinition_OperationType ∷
  Tuple
    ( { operationType ∷ OperationType, name ∷ (Maybe String), variableDefinitions ∷ (Maybe VariableDefinitions), directives ∷ (Maybe Directives), selectionSet ∷ SelectionSet } → OperationDefinition
    )
    ( OperationDefinition →
      Maybe { operationType ∷ OperationType, name ∷ (Maybe String), variableDefinitions ∷ (Maybe VariableDefinitions), directives ∷ (Maybe Directives), selectionSet ∷ SelectionSet }
    )
_OperationDefinition_OperationType =
  Tuple OperationDefinition_OperationType
    ( case _ of
        OperationDefinition_OperationType a → Just a
        _ → Nothing
    )

data OperationDefinition
  = OperationDefinition_SelectionSet SelectionSet
  | OperationDefinition_OperationType { operationType ∷ OperationType, name ∷ (Maybe String), variableDefinitions ∷ (Maybe VariableDefinitions), directives ∷ (Maybe Directives), selectionSet ∷ SelectionSet }

derive instance operationTypeGeneric ∷ Generic OperationType _

instance operationTypeShow ∷ Show OperationType where
  show v = genericShow v

derive instance operationTypeEq ∷ Eq OperationType

_Query ∷
  Tuple
    ( Unit → OperationType
    )
    ( OperationType →
      Maybe Unit
    )
_Query =
  Tuple (\_ → Query)
    ( case _ of
        Query → Just unit
        _ → Nothing
    )

_Mutation ∷
  Tuple
    ( Unit → OperationType
    )
    ( OperationType →
      Maybe Unit
    )
_Mutation =
  Tuple (\_ → Mutation)
    ( case _ of
        Mutation → Just unit
        _ → Nothing
    )

_Subscription ∷
  Tuple
    ( Unit → OperationType
    )
    ( OperationType →
      Maybe Unit
    )
_Subscription =
  Tuple (\_ → Subscription)
    ( case _ of
        Subscription → Just unit
        _ → Nothing
    )

data OperationType
  = Query
  | Mutation
  | Subscription

derive instance selectionSetGeneric ∷ Generic SelectionSet _

instance selectionSetShow ∷ Show SelectionSet where
  show v = genericShow v

derive instance selectionSetEq ∷ Eq SelectionSet

_SelectionSet ∷
  Tuple
    ( (List Selection) → SelectionSet
    )
    ( SelectionSet →
      Maybe (List Selection)
    )
_SelectionSet =
  Tuple SelectionSet
    ( case _ of
        SelectionSet a → Just a
    )

data SelectionSet
  = SelectionSet (List Selection)

derive instance selectionGeneric ∷ Generic Selection _

instance selectionShow ∷ Show Selection where
  show v = genericShow v

derive instance selectionEq ∷ Eq Selection

_Selection_Field ∷
  Tuple
    ( Field → Selection
    )
    ( Selection →
      Maybe Field
    )
_Selection_Field =
  Tuple Selection_Field
    ( case _ of
        Selection_Field a → Just a
        _ → Nothing
    )

_Selection_FragmentSpread ∷
  Tuple
    ( FragmentSpread → Selection
    )
    ( Selection →
      Maybe FragmentSpread
    )
_Selection_FragmentSpread =
  Tuple Selection_FragmentSpread
    ( case _ of
        Selection_FragmentSpread a → Just a
        _ → Nothing
    )

_Selection_InlineFragment ∷
  Tuple
    ( InlineFragment → Selection
    )
    ( Selection →
      Maybe InlineFragment
    )
_Selection_InlineFragment =
  Tuple Selection_InlineFragment
    ( case _ of
        Selection_InlineFragment a → Just a
        _ → Nothing
    )

data Selection
  = Selection_Field Field
  | Selection_FragmentSpread FragmentSpread
  | Selection_InlineFragment InlineFragment

derive instance fieldGeneric ∷ Generic Field _

instance fieldShow ∷ Show Field where
  show v = genericShow v

derive instance fieldEq ∷ Eq Field

_Field ∷
  Tuple
    ( { alias ∷ (Maybe String), name ∷ String, arguments ∷ (Maybe Arguments), directives ∷ (Maybe Directives), selectionSet ∷ (Maybe SelectionSet) } → Field
    )
    ( Field →
      Maybe { alias ∷ (Maybe String), name ∷ String, arguments ∷ (Maybe Arguments), directives ∷ (Maybe Directives), selectionSet ∷ (Maybe SelectionSet) }
    )
_Field =
  Tuple Field
    ( case _ of
        Field a → Just a
    )

data Field
  = Field { alias ∷ (Maybe String), name ∷ String, arguments ∷ (Maybe Arguments), directives ∷ (Maybe Directives), selectionSet ∷ (Maybe SelectionSet) }

derive instance argumentsGeneric ∷ Generic Arguments _

instance argumentsShow ∷ Show Arguments where
  show v = genericShow v

derive instance argumentsEq ∷ Eq Arguments

_Arguments ∷
  Tuple
    ( (List Argument) → Arguments
    )
    ( Arguments →
      Maybe (List Argument)
    )
_Arguments =
  Tuple Arguments
    ( case _ of
        Arguments a → Just a
    )

data Arguments
  = Arguments (List Argument)

derive instance argumentGeneric ∷ Generic Argument _

instance argumentShow ∷ Show Argument where
  show v = genericShow v

derive instance argumentEq ∷ Eq Argument

_Argument ∷
  Tuple
    ( { name :: String, value :: Value } → Argument
    )
    ( Argument →
      Maybe { name :: String, value :: Value }
    )
_Argument =
  Tuple Argument
    ( case _ of
        Argument a → Just a
    )

data Argument
  = Argument { name :: String, value :: Value }

derive instance fragmentSpreadGeneric ∷ Generic FragmentSpread _

instance fragmentSpreadShow ∷ Show FragmentSpread where
  show v = genericShow v

derive instance fragmentSpreadEq ∷ Eq FragmentSpread

_FragmentSpread ∷
  Tuple
    ( { fragmentName ∷ String, directives ∷ Maybe Directives } → FragmentSpread
    )
    ( FragmentSpread →
      Maybe { fragmentName ∷ String, directives ∷ Maybe Directives }
    )
_FragmentSpread =
  Tuple FragmentSpread
    ( case _ of
        FragmentSpread a → Just a
    )

data FragmentSpread
  = FragmentSpread { fragmentName ∷ String, directives ∷ Maybe Directives }

derive instance inlineFragmentGeneric ∷ Generic InlineFragment _

instance inlineFragmentShow ∷ Show InlineFragment where
  show v = genericShow v

derive instance inlineFragmentEq ∷ Eq InlineFragment

_InlineFragment ∷
  Tuple
    ( { typeCondition ∷ (Maybe TypeCondition), directives ∷ (Maybe Directives), selectionSet ∷ SelectionSet } → InlineFragment
    )
    ( InlineFragment →
      Maybe { typeCondition ∷ (Maybe TypeCondition), directives ∷ (Maybe Directives), selectionSet ∷ SelectionSet }
    )
_InlineFragment =
  Tuple InlineFragment
    ( case _ of
        InlineFragment a → Just a
    )

data InlineFragment
  = InlineFragment { typeCondition ∷ (Maybe TypeCondition), directives ∷ (Maybe Directives), selectionSet ∷ SelectionSet }

derive instance fragmentDefinitionGeneric ∷ Generic FragmentDefinition _

instance fragmentDefinitionShow ∷ Show FragmentDefinition where
  show v = genericShow v

derive instance fragmentDefinitionEq ∷ Eq FragmentDefinition

_FragmentDefinition ∷
  Tuple
    ( { fragmentName ∷ String, typeCondition ∷ TypeCondition, directives ∷ (Maybe Directives), selectionSet ∷ SelectionSet } → FragmentDefinition
    )
    ( FragmentDefinition →
      Maybe { fragmentName ∷ String, typeCondition ∷ TypeCondition, directives ∷ (Maybe Directives), selectionSet ∷ SelectionSet }
    )
_FragmentDefinition =
  Tuple FragmentDefinition
    ( case _ of
        FragmentDefinition a → Just a
    )

data FragmentDefinition
  = FragmentDefinition { fragmentName ∷ String, typeCondition ∷ TypeCondition, directives ∷ (Maybe Directives), selectionSet ∷ SelectionSet }

derive instance typeConditionGeneric ∷ Generic TypeCondition _

instance typeConditionShow ∷ Show TypeCondition where
  show v = genericShow v

derive instance typeConditionEq ∷ Eq TypeCondition

_TypeCondition ∷
  Tuple
    ( NamedType → TypeCondition
    )
    ( TypeCondition →
      Maybe NamedType
    )
_TypeCondition =
  Tuple TypeCondition
    ( case _ of
        TypeCondition a → Just a
    )

data TypeCondition
  = TypeCondition NamedType

derive instance valueGeneric ∷ Generic Value _

instance valueShow ∷ Show Value where
  show v = genericShow v

derive instance valueEq ∷ Eq Value

_Value_Variable ∷
  Tuple
    ( Variable → Value
    )
    ( Value →
      Maybe Variable
    )
_Value_Variable =
  Tuple Value_Variable
    ( case _ of
        Value_Variable a → Just a
        _ → Nothing
    )

_Value_IntValue ∷
  Tuple
    ( IntValue → Value
    )
    ( Value →
      Maybe IntValue
    )
_Value_IntValue =
  Tuple Value_IntValue
    ( case _ of
        Value_IntValue a → Just a
        _ → Nothing
    )

_Value_FloatValue ∷
  Tuple
    ( FloatValue → Value
    )
    ( Value →
      Maybe FloatValue
    )
_Value_FloatValue =
  Tuple Value_FloatValue
    ( case _ of
        Value_FloatValue a → Just a
        _ → Nothing
    )

_Value_StringValue ∷
  Tuple
    ( StringValue → Value
    )
    ( Value →
      Maybe StringValue
    )
_Value_StringValue =
  Tuple Value_StringValue
    ( case _ of
        Value_StringValue a → Just a
        _ → Nothing
    )

_Value_BooleanValue ∷
  Tuple
    ( BooleanValue → Value
    )
    ( Value →
      Maybe BooleanValue
    )
_Value_BooleanValue =
  Tuple Value_BooleanValue
    ( case _ of
        Value_BooleanValue a → Just a
        _ → Nothing
    )

_Value_NullValue ∷
  Tuple
    ( NullValue → Value
    )
    ( Value →
      Maybe NullValue
    )
_Value_NullValue =
  Tuple Value_NullValue
    ( case _ of
        Value_NullValue a → Just a
        _ → Nothing
    )

_Value_EnumValue ∷
  Tuple
    ( EnumValue → Value
    )
    ( Value →
      Maybe EnumValue
    )
_Value_EnumValue =
  Tuple Value_EnumValue
    ( case _ of
        Value_EnumValue a → Just a
        _ → Nothing
    )

_Value_ListValue ∷
  Tuple
    ( ListValue → Value
    )
    ( Value →
      Maybe ListValue
    )
_Value_ListValue =
  Tuple Value_ListValue
    ( case _ of
        Value_ListValue a → Just a
        _ → Nothing
    )

_Value_ObjectValue ∷
  Tuple
    ( ObjectValue → Value
    )
    ( Value →
      Maybe ObjectValue
    )
_Value_ObjectValue =
  Tuple Value_ObjectValue
    ( case _ of
        Value_ObjectValue a → Just a
        _ → Nothing
    )

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

_IntValue ∷
  Tuple
    ( Int → IntValue
    )
    ( IntValue →
      Maybe Int
    )
_IntValue =
  Tuple IntValue
    ( case _ of
        IntValue a → Just a
    )

data IntValue
  = IntValue Int

derive instance floatValueGeneric ∷ Generic FloatValue _

instance floatValueShow ∷ Show FloatValue where
  show v = genericShow v

derive instance floatValueEq ∷ Eq FloatValue

_FloatValue ∷
  Tuple
    ( Number → FloatValue
    )
    ( FloatValue →
      Maybe Number
    )
_FloatValue =
  Tuple FloatValue
    ( case _ of
        FloatValue a → Just a
    )

data FloatValue
  = FloatValue Number

derive instance booleanValueGeneric ∷ Generic BooleanValue _

instance booleanValueShow ∷ Show BooleanValue where
  show v = genericShow v

derive instance booleanValueEq ∷ Eq BooleanValue

_BooleanValue ∷
  Tuple
    ( Boolean → BooleanValue
    )
    ( BooleanValue →
      Maybe Boolean
    )
_BooleanValue =
  Tuple BooleanValue
    ( case _ of
        BooleanValue a → Just a
    )

data BooleanValue
  = BooleanValue Boolean

derive instance stringValueGeneric ∷ Generic StringValue _

instance stringValueShow ∷ Show StringValue where
  show v = genericShow v

derive instance stringValueEq ∷ Eq StringValue

_StringValue ∷
  Tuple
    ( String → StringValue
    )
    ( StringValue →
      Maybe String
    )
_StringValue =
  Tuple StringValue
    ( case _ of
        StringValue a → Just a
    )

data StringValue
  = StringValue String

derive instance nullValueGeneric ∷ Generic NullValue _

instance nullValueShow ∷ Show NullValue where
  show v = genericShow v

derive instance nullValueEq ∷ Eq NullValue

_NullValue ∷
  Tuple
    ( Unit → NullValue
    )
    ( NullValue →
      Maybe Unit
    )
_NullValue =
  Tuple (\_ → NullValue)
    ( case _ of
        NullValue → Just unit
    )

data NullValue
  = NullValue

derive instance enumValueGeneric ∷ Generic EnumValue _

instance enumValueShow ∷ Show EnumValue where
  show v = genericShow v

derive instance enumValueEq ∷ Eq EnumValue

_EnumValue ∷
  Tuple
    ( String → EnumValue
    )
    ( EnumValue →
      Maybe String
    )
_EnumValue =
  Tuple EnumValue
    ( case _ of
        EnumValue a → Just a
    )

data EnumValue
  = EnumValue String

derive instance listValueGeneric ∷ Generic ListValue _

instance listValueShow ∷ Show ListValue where
  show v = genericShow v

derive instance listValueEq ∷ Eq ListValue

_ListValue ∷
  Tuple
    ( (List Value) → ListValue
    )
    ( ListValue →
      Maybe (List Value)
    )
_ListValue =
  Tuple ListValue
    ( case _ of
        ListValue a → Just a
    )

data ListValue
  = ListValue (List Value)

derive instance objectValueGeneric ∷ Generic ObjectValue _

instance objectValueShow ∷ Show ObjectValue where
  show v = genericShow v

derive instance objectValueEq ∷ Eq ObjectValue

_ObjectValue ∷
  Tuple
    ( (List Argument) → ObjectValue
    )
    ( ObjectValue →
      Maybe (List Argument)
    )
_ObjectValue =
  Tuple ObjectValue
    ( case _ of
        ObjectValue a → Just a
    )

data ObjectValue
  = ObjectValue (List Argument)

derive instance variableDefinitionsGeneric ∷ Generic VariableDefinitions _

instance variableDefinitionsShow ∷ Show VariableDefinitions where
  show v = genericShow v

derive instance variableDefinitionsEq ∷ Eq VariableDefinitions

_VariableDefinitions ∷
  Tuple
    ( (List VariableDefinition) → VariableDefinitions
    )
    ( VariableDefinitions →
      Maybe (List VariableDefinition)
    )
_VariableDefinitions =
  Tuple VariableDefinitions
    ( case _ of
        VariableDefinitions a → Just a
    )

data VariableDefinitions
  = VariableDefinitions (List VariableDefinition)

derive instance variableDefinitionGeneric ∷ Generic VariableDefinition _

instance variableDefinitionShow ∷ Show VariableDefinition where
  show v = genericShow v

derive instance variableDefinitionEq ∷ Eq VariableDefinition

_VariableDefinition ∷
  Tuple
    ( { variable ∷ Variable, type ∷ Type, defaultValue ∷ (Maybe DefaultValue) } → VariableDefinition
    )
    ( VariableDefinition →
      Maybe { variable ∷ Variable, type ∷ Type, defaultValue ∷ (Maybe DefaultValue) }
    )
_VariableDefinition =
  Tuple VariableDefinition
    ( case _ of
        VariableDefinition a → Just a
    )

data VariableDefinition
  = VariableDefinition { variable ∷ Variable, type ∷ Type, defaultValue ∷ (Maybe DefaultValue) }

derive instance variableGeneric ∷ Generic Variable _

instance variableShow ∷ Show Variable where
  show v = genericShow v

derive instance variableEq ∷ Eq Variable

_Variable ∷
  Tuple
    ( String → Variable
    )
    ( Variable →
      Maybe String
    )
_Variable =
  Tuple Variable
    ( case _ of
        Variable a → Just a
    )

data Variable
  = Variable String

derive instance defaultValueGeneric ∷ Generic DefaultValue _

instance defaultValueShow ∷ Show DefaultValue where
  show v = genericShow v

derive instance defaultValueEq ∷ Eq DefaultValue

_DefaultValue ∷
  Tuple
    ( Value → DefaultValue
    )
    ( DefaultValue →
      Maybe Value
    )
_DefaultValue =
  Tuple DefaultValue
    ( case _ of
        DefaultValue a → Just a
    )

data DefaultValue
  = DefaultValue Value

derive instance typeGeneric ∷ Generic Type _

instance typeShow ∷ Show Type where
  show v = genericShow v

derive instance typeEq ∷ Eq Type

_Type_NamedType ∷
  Tuple
    ( NamedType → Type
    )
    ( Type →
      Maybe NamedType
    )
_Type_NamedType =
  Tuple Type_NamedType
    ( case _ of
        Type_NamedType a → Just a
        _ → Nothing
    )

_Type_ListType ∷
  Tuple
    ( ListType → Type
    )
    ( Type →
      Maybe ListType
    )
_Type_ListType =
  Tuple Type_ListType
    ( case _ of
        Type_ListType a → Just a
        _ → Nothing
    )

_Type_NonNullType ∷
  Tuple
    ( NonNullType → Type
    )
    ( Type →
      Maybe NonNullType
    )
_Type_NonNullType =
  Tuple Type_NonNullType
    ( case _ of
        Type_NonNullType a → Just a
        _ → Nothing
    )

data Type
  = Type_NamedType NamedType
  | Type_ListType ListType
  | Type_NonNullType NonNullType

derive instance namedTypeGeneric ∷ Generic NamedType _

instance namedTypeShow ∷ Show NamedType where
  show v = genericShow v

derive instance namedTypeEq ∷ Eq NamedType

_NamedType ∷
  Tuple
    ( String → NamedType
    )
    ( NamedType →
      Maybe String
    )
_NamedType =
  Tuple NamedType
    ( case _ of
        NamedType a → Just a
    )

data NamedType
  = NamedType String

derive instance listTypeGeneric ∷ Generic ListType _

instance listTypeShow ∷ Show ListType where
  show v = genericShow v

derive instance listTypeEq ∷ Eq ListType

_ListType ∷
  Tuple
    ( (List Type) → ListType
    )
    ( ListType →
      Maybe (List Type)
    )
_ListType =
  Tuple ListType
    ( case _ of
        ListType a → Just a
    )

data ListType
  = ListType (List Type)

derive instance nonNullTypeGeneric ∷ Generic NonNullType _

instance nonNullTypeShow ∷ Show NonNullType where
  show v = genericShow v

derive instance nonNullTypeEq ∷ Eq NonNullType

_NonNullType_NamedType ∷
  Tuple
    ( NamedType → NonNullType
    )
    ( NonNullType →
      Maybe NamedType
    )
_NonNullType_NamedType =
  Tuple NonNullType_NamedType
    ( case _ of
        NonNullType_NamedType a → Just a
        _ → Nothing
    )

_NonNullType_ListType ∷
  Tuple
    ( ListType → NonNullType
    )
    ( NonNullType →
      Maybe ListType
    )
_NonNullType_ListType =
  Tuple NonNullType_ListType
    ( case _ of
        NonNullType_ListType a → Just a
        _ → Nothing
    )

data NonNullType
  = NonNullType_NamedType NamedType
  | NonNullType_ListType ListType

derive instance directivesGeneric ∷ Generic Directives _

instance directivesShow ∷ Show Directives where
  show v = genericShow v

derive instance directivesEq ∷ Eq Directives

_Directives ∷
  Tuple
    ( (List Directive) → Directives
    )
    ( Directives →
      Maybe (List Directive)
    )
_Directives =
  Tuple Directives
    ( case _ of
        Directives a → Just a
    )

data Directives
  = Directives (List Directive)

derive instance directiveGeneric ∷ Generic Directive _

instance directiveShow ∷ Show Directive where
  show v = genericShow v

derive instance directiveEq ∷ Eq Directive

_Directive ∷
  Tuple
    ( { name ∷ String, arguments ∷ (Maybe Arguments) } → Directive
    )
    ( Directive →
      Maybe { name ∷ String, arguments ∷ (Maybe Arguments) }
    )
_Directive =
  Tuple Directive
    ( case _ of
        Directive a → Just a
    )

data Directive
  = Directive { name ∷ String, arguments ∷ (Maybe Arguments) }

derive instance typeSystemDefinitionGeneric ∷ Generic TypeSystemDefinition _

instance typeSystemDefinitionShow ∷ Show TypeSystemDefinition where
  show v = genericShow v

derive instance typeSystemDefinitionEq ∷ Eq TypeSystemDefinition

_TypeSystemDefinition_SchemaDefinition ∷
  Tuple
    ( SchemaDefinition → TypeSystemDefinition
    )
    ( TypeSystemDefinition →
      Maybe SchemaDefinition
    )
_TypeSystemDefinition_SchemaDefinition =
  Tuple TypeSystemDefinition_SchemaDefinition
    ( case _ of
        TypeSystemDefinition_SchemaDefinition a → Just a
        _ → Nothing
    )

_TypeSystemDefinition_TypeDefinition ∷
  Tuple
    ( TypeDefinition → TypeSystemDefinition
    )
    ( TypeSystemDefinition →
      Maybe TypeDefinition
    )
_TypeSystemDefinition_TypeDefinition =
  Tuple TypeSystemDefinition_TypeDefinition
    ( case _ of
        TypeSystemDefinition_TypeDefinition a → Just a
        _ → Nothing
    )

_TypeSystemDefinition_DirectiveDefinition ∷
  Tuple
    ( DirectiveDefinition → TypeSystemDefinition
    )
    ( TypeSystemDefinition →
      Maybe DirectiveDefinition
    )
_TypeSystemDefinition_DirectiveDefinition =
  Tuple TypeSystemDefinition_DirectiveDefinition
    ( case _ of
        TypeSystemDefinition_DirectiveDefinition a → Just a
        _ → Nothing
    )

data TypeSystemDefinition
  = TypeSystemDefinition_SchemaDefinition SchemaDefinition
  | TypeSystemDefinition_TypeDefinition TypeDefinition
  | TypeSystemDefinition_DirectiveDefinition DirectiveDefinition

derive instance typeSystemExtensionGeneric ∷ Generic TypeSystemExtension _

instance typeSystemExtensionShow ∷ Show TypeSystemExtension where
  show v = genericShow v

derive instance typeSystemExtensionEq ∷ Eq TypeSystemExtension

_TypeSystemExtension_SchemaExtension ∷
  Tuple
    ( SchemaExtension → TypeSystemExtension
    )
    ( TypeSystemExtension →
      Maybe SchemaExtension
    )
_TypeSystemExtension_SchemaExtension =
  Tuple TypeSystemExtension_SchemaExtension
    ( case _ of
        TypeSystemExtension_SchemaExtension a → Just a
        _ → Nothing
    )

_TypeSystemExtension_TypeExtension ∷
  Tuple
    ( TypeExtension → TypeSystemExtension
    )
    ( TypeSystemExtension →
      Maybe TypeExtension
    )
_TypeSystemExtension_TypeExtension =
  Tuple TypeSystemExtension_TypeExtension
    ( case _ of
        TypeSystemExtension_TypeExtension a → Just a
        _ → Nothing
    )

data TypeSystemExtension
  = TypeSystemExtension_SchemaExtension SchemaExtension
  | TypeSystemExtension_TypeExtension TypeExtension

derive instance schemaDefinitionGeneric ∷ Generic SchemaDefinition _

instance schemaDefinitionShow ∷ Show SchemaDefinition where
  show v = genericShow v

derive instance schemaDefinitionEq ∷ Eq SchemaDefinition

_SchemaDefinition ∷
  Tuple
    ( { directives ∷ (Maybe Directives), rootOperationTypeDefinition ∷ (List RootOperationTypeDefinition) } → SchemaDefinition
    )
    ( SchemaDefinition →
      Maybe { directives ∷ (Maybe Directives), rootOperationTypeDefinition ∷ (List RootOperationTypeDefinition) }
    )
_SchemaDefinition =
  Tuple SchemaDefinition
    ( case _ of
        SchemaDefinition a → Just a
    )

data SchemaDefinition
  = SchemaDefinition { directives ∷ (Maybe Directives), rootOperationTypeDefinition ∷ (List RootOperationTypeDefinition) }

derive instance rootOperationTypeDefinitionGeneric ∷ Generic RootOperationTypeDefinition _

instance rootOperationTypeDefinitionShow ∷ Show RootOperationTypeDefinition where
  show v = genericShow v

derive instance rootOperationTypeDefinitionEq ∷ Eq RootOperationTypeDefinition

_RootOperationTypeDefinition ∷
  Tuple
    ( { operationType ∷ OperationType, namedType ∷ NamedType } → RootOperationTypeDefinition
    )
    ( RootOperationTypeDefinition →
      Maybe { operationType ∷ OperationType, namedType ∷ NamedType }
    )
_RootOperationTypeDefinition =
  Tuple RootOperationTypeDefinition
    ( case _ of
        RootOperationTypeDefinition a → Just a
    )

data RootOperationTypeDefinition
  = RootOperationTypeDefinition { operationType ∷ OperationType, namedType ∷ NamedType }

derive instance schemaExtensionGeneric ∷ Generic SchemaExtension _

instance schemaExtensionShow ∷ Show SchemaExtension where
  show v = genericShow v

derive instance schemaExtensionEq ∷ Eq SchemaExtension

_SchemaExtension_With_OperationTypeDefinition ∷
  Tuple
    ( { directives ∷ (Maybe Directives), operationTypesDefinition ∷ (List OperationTypeDefinition) } → SchemaExtension
    )
    ( SchemaExtension →
      Maybe { directives ∷ (Maybe Directives), operationTypesDefinition ∷ (List OperationTypeDefinition) }
    )
_SchemaExtension_With_OperationTypeDefinition =
  Tuple SchemaExtension_With_OperationTypeDefinition
    ( case _ of
        SchemaExtension_With_OperationTypeDefinition a → Just a
        _ → Nothing
    )

_SchemaExtension_With_Directives ∷
  Tuple
    ( { directives ∷ Directives } → SchemaExtension
    )
    ( SchemaExtension →
      Maybe { directives ∷ Directives }
    )
_SchemaExtension_With_Directives =
  Tuple SchemaExtension_With_Directives
    ( case _ of
        SchemaExtension_With_Directives a → Just a
        _ → Nothing
    )

data SchemaExtension
  = SchemaExtension_With_OperationTypeDefinition { directives ∷ (Maybe Directives), operationTypesDefinition ∷ (List OperationTypeDefinition) }
  | SchemaExtension_With_Directives { directives ∷ Directives }

derive instance operationTypeDefinitionGeneric ∷ Generic OperationTypeDefinition _

instance operationTypeDefinitionShow ∷ Show OperationTypeDefinition where
  show v = genericShow v

derive instance operationTypeDefinitionEq ∷ Eq OperationTypeDefinition

_OperationTypeDefinition ∷
  Tuple
    ( { operationType ∷ OperationType, namedType ∷ NamedType } → OperationTypeDefinition
    )
    ( OperationTypeDefinition →
      Maybe { operationType ∷ OperationType, namedType ∷ NamedType }
    )
_OperationTypeDefinition =
  Tuple OperationTypeDefinition
    ( case _ of
        OperationTypeDefinition a → Just a
    )

data OperationTypeDefinition
  = OperationTypeDefinition { operationType ∷ OperationType, namedType ∷ NamedType }

derive instance typeDefinitionGeneric ∷ Generic TypeDefinition _

instance typeDefinitionShow ∷ Show TypeDefinition where
  show v = genericShow v

derive instance typeDefinitionEq ∷ Eq TypeDefinition

_TypeDefinition_ScalarTypeDefinition ∷
  Tuple
    ( ScalarTypeDefinition → TypeDefinition
    )
    ( TypeDefinition →
      Maybe ScalarTypeDefinition
    )
_TypeDefinition_ScalarTypeDefinition =
  Tuple TypeDefinition_ScalarTypeDefinition
    ( case _ of
        TypeDefinition_ScalarTypeDefinition a → Just a
        _ → Nothing
    )

_TypeDefinition_ObjectTypeDefinition ∷
  Tuple
    ( ObjectTypeDefinition → TypeDefinition
    )
    ( TypeDefinition →
      Maybe ObjectTypeDefinition
    )
_TypeDefinition_ObjectTypeDefinition =
  Tuple TypeDefinition_ObjectTypeDefinition
    ( case _ of
        TypeDefinition_ObjectTypeDefinition a → Just a
        _ → Nothing
    )

_TypeDefinition_InterfaceTypeDefinition ∷
  Tuple
    ( InterfaceTypeDefinition → TypeDefinition
    )
    ( TypeDefinition →
      Maybe InterfaceTypeDefinition
    )
_TypeDefinition_InterfaceTypeDefinition =
  Tuple TypeDefinition_InterfaceTypeDefinition
    ( case _ of
        TypeDefinition_InterfaceTypeDefinition a → Just a
        _ → Nothing
    )

_TypeDefinition_UnionTypeDefinition ∷
  Tuple
    ( UnionTypeDefinition → TypeDefinition
    )
    ( TypeDefinition →
      Maybe UnionTypeDefinition
    )
_TypeDefinition_UnionTypeDefinition =
  Tuple TypeDefinition_UnionTypeDefinition
    ( case _ of
        TypeDefinition_UnionTypeDefinition a → Just a
        _ → Nothing
    )

_TypeDefinition_EnumTypeDefinition ∷
  Tuple
    ( EnumTypeDefinition → TypeDefinition
    )
    ( TypeDefinition →
      Maybe EnumTypeDefinition
    )
_TypeDefinition_EnumTypeDefinition =
  Tuple TypeDefinition_EnumTypeDefinition
    ( case _ of
        TypeDefinition_EnumTypeDefinition a → Just a
        _ → Nothing
    )

_TypeDefinition_InputObjectTypeDefinition ∷
  Tuple
    ( InputObjectTypeDefinition → TypeDefinition
    )
    ( TypeDefinition →
      Maybe InputObjectTypeDefinition
    )
_TypeDefinition_InputObjectTypeDefinition =
  Tuple TypeDefinition_InputObjectTypeDefinition
    ( case _ of
        TypeDefinition_InputObjectTypeDefinition a → Just a
        _ → Nothing
    )

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

_TypeExtension_ScalarTypeExtension ∷
  Tuple
    ( ScalarTypeExtension → TypeExtension
    )
    ( TypeExtension →
      Maybe ScalarTypeExtension
    )
_TypeExtension_ScalarTypeExtension =
  Tuple TypeExtension_ScalarTypeExtension
    ( case _ of
        TypeExtension_ScalarTypeExtension a → Just a
        _ → Nothing
    )

_TypeExtension_ObjectTypeExtension ∷
  Tuple
    ( ObjectTypeExtension → TypeExtension
    )
    ( TypeExtension →
      Maybe ObjectTypeExtension
    )
_TypeExtension_ObjectTypeExtension =
  Tuple TypeExtension_ObjectTypeExtension
    ( case _ of
        TypeExtension_ObjectTypeExtension a → Just a
        _ → Nothing
    )

_TypeExtension_InterfaceTypeExtension ∷
  Tuple
    ( InterfaceTypeExtension → TypeExtension
    )
    ( TypeExtension →
      Maybe InterfaceTypeExtension
    )
_TypeExtension_InterfaceTypeExtension =
  Tuple TypeExtension_InterfaceTypeExtension
    ( case _ of
        TypeExtension_InterfaceTypeExtension a → Just a
        _ → Nothing
    )

_TypeExtension_UnionTypeExtension ∷
  Tuple
    ( UnionTypeExtension → TypeExtension
    )
    ( TypeExtension →
      Maybe UnionTypeExtension
    )
_TypeExtension_UnionTypeExtension =
  Tuple TypeExtension_UnionTypeExtension
    ( case _ of
        TypeExtension_UnionTypeExtension a → Just a
        _ → Nothing
    )

_TypeExtension_EnumTypeExtension ∷
  Tuple
    ( EnumTypeExtension → TypeExtension
    )
    ( TypeExtension →
      Maybe EnumTypeExtension
    )
_TypeExtension_EnumTypeExtension =
  Tuple TypeExtension_EnumTypeExtension
    ( case _ of
        TypeExtension_EnumTypeExtension a → Just a
        _ → Nothing
    )

_TypeExtension_InputObjectTypeExtension ∷
  Tuple
    ( InputObjectTypeExtension → TypeExtension
    )
    ( TypeExtension →
      Maybe InputObjectTypeExtension
    )
_TypeExtension_InputObjectTypeExtension =
  Tuple TypeExtension_InputObjectTypeExtension
    ( case _ of
        TypeExtension_InputObjectTypeExtension a → Just a
        _ → Nothing
    )

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

_ScalarTypeDefinition ∷
  Tuple
    ( { description ∷ (Maybe String), name ∷ String, directives ∷ (Maybe Directives) } → ScalarTypeDefinition
    )
    ( ScalarTypeDefinition →
      Maybe { description ∷ (Maybe String), name ∷ String, directives ∷ (Maybe Directives) }
    )
_ScalarTypeDefinition =
  Tuple ScalarTypeDefinition
    ( case _ of
        ScalarTypeDefinition a → Just a
    )

data ScalarTypeDefinition
  = ScalarTypeDefinition { description ∷ (Maybe String), name ∷ String, directives ∷ (Maybe Directives) }

derive instance scalarTypeExtensionGeneric ∷ Generic ScalarTypeExtension _

instance scalarTypeExtensionShow ∷ Show ScalarTypeExtension where
  show v = genericShow v

derive instance scalarTypeExtensionEq ∷ Eq ScalarTypeExtension

_ScalarTypeExtension ∷
  Tuple
    ( { name ∷ String, directives ∷ Directives } → ScalarTypeExtension
    )
    ( ScalarTypeExtension →
      Maybe { name ∷ String, directives ∷ Directives }
    )
_ScalarTypeExtension =
  Tuple ScalarTypeExtension
    ( case _ of
        ScalarTypeExtension a → Just a
    )

data ScalarTypeExtension
  = ScalarTypeExtension { name ∷ String, directives ∷ Directives }

derive instance objectTypeDefinitionGeneric ∷ Generic ObjectTypeDefinition _

instance objectTypeDefinitionShow ∷ Show ObjectTypeDefinition where
  show v = genericShow v

derive instance objectTypeDefinitionEq ∷ Eq ObjectTypeDefinition

_ObjectTypeDefinition ∷
  Tuple
    ( { description ∷ (Maybe String), name ∷ String, implementsInterfaces ∷ (Maybe ImplementsInterfaces), directives ∷ (Maybe Directives), fieldsDefinition ∷ (Maybe FieldsDefinition) } → ObjectTypeDefinition
    )
    ( ObjectTypeDefinition →
      Maybe { description ∷ (Maybe String), name ∷ String, implementsInterfaces ∷ (Maybe ImplementsInterfaces), directives ∷ (Maybe Directives), fieldsDefinition ∷ (Maybe FieldsDefinition) }
    )
_ObjectTypeDefinition =
  Tuple ObjectTypeDefinition
    ( case _ of
        ObjectTypeDefinition a → Just a
    )

data ObjectTypeDefinition
  = ObjectTypeDefinition { description ∷ (Maybe String), name ∷ String, implementsInterfaces ∷ (Maybe ImplementsInterfaces), directives ∷ (Maybe Directives), fieldsDefinition ∷ (Maybe FieldsDefinition) }

derive instance objectTypeExtensionGeneric ∷ Generic ObjectTypeExtension _

instance objectTypeExtensionShow ∷ Show ObjectTypeExtension where
  show v = genericShow v

derive instance objectTypeExtensionEq ∷ Eq ObjectTypeExtension

_ObjectTypeExtension_With_FieldsDefinition ∷
  Tuple
    ( { name ∷ String, implementsInterfaces ∷ (Maybe ImplementsInterfaces), directives ∷ (Maybe Directives), fieldsDefinition ∷ FieldsDefinition } → ObjectTypeExtension
    )
    ( ObjectTypeExtension →
      Maybe { name ∷ String, implementsInterfaces ∷ (Maybe ImplementsInterfaces), directives ∷ (Maybe Directives), fieldsDefinition ∷ FieldsDefinition }
    )
_ObjectTypeExtension_With_FieldsDefinition =
  Tuple ObjectTypeExtension_With_FieldsDefinition
    ( case _ of
        ObjectTypeExtension_With_FieldsDefinition a → Just a
        _ → Nothing
    )

_ObjectTypeExtension_With_Directives ∷
  Tuple
    ( { name ∷ String, implementsInterfaces ∷ (Maybe ImplementsInterfaces), directives ∷ Directives } → ObjectTypeExtension
    )
    ( ObjectTypeExtension →
      Maybe { name ∷ String, implementsInterfaces ∷ (Maybe ImplementsInterfaces), directives ∷ Directives }
    )
_ObjectTypeExtension_With_Directives =
  Tuple ObjectTypeExtension_With_Directives
    ( case _ of
        ObjectTypeExtension_With_Directives a → Just a
        _ → Nothing
    )

_ObjectTypeExtension_With_ImplementsInterfaces ∷
  Tuple
    ( { name ∷ String, implementsInterfaces ∷ ImplementsInterfaces } → ObjectTypeExtension
    )
    ( ObjectTypeExtension →
      Maybe { name ∷ String, implementsInterfaces ∷ ImplementsInterfaces }
    )
_ObjectTypeExtension_With_ImplementsInterfaces =
  Tuple ObjectTypeExtension_With_ImplementsInterfaces
    ( case _ of
        ObjectTypeExtension_With_ImplementsInterfaces a → Just a
        _ → Nothing
    )

data ObjectTypeExtension
  = ObjectTypeExtension_With_FieldsDefinition { name ∷ String, implementsInterfaces ∷ (Maybe ImplementsInterfaces), directives ∷ (Maybe Directives), fieldsDefinition ∷ FieldsDefinition }
  | ObjectTypeExtension_With_Directives { name ∷ String, implementsInterfaces ∷ (Maybe ImplementsInterfaces), directives ∷ Directives }
  | ObjectTypeExtension_With_ImplementsInterfaces { name ∷ String, implementsInterfaces ∷ ImplementsInterfaces }

derive instance implementsInterfacesGeneric ∷ Generic ImplementsInterfaces _

instance implementsInterfacesShow ∷ Show ImplementsInterfaces where
  show v = genericShow v

derive instance implementsInterfacesEq ∷ Eq ImplementsInterfaces

_ImplementsInterfaces ∷
  Tuple
    ( (List NamedType) → ImplementsInterfaces
    )
    ( ImplementsInterfaces →
      Maybe (List NamedType)
    )
_ImplementsInterfaces =
  Tuple ImplementsInterfaces
    ( case _ of
        ImplementsInterfaces a → Just a
    )

data ImplementsInterfaces
  = ImplementsInterfaces (List NamedType)

derive instance fieldsDefinitionGeneric ∷ Generic FieldsDefinition _

instance fieldsDefinitionShow ∷ Show FieldsDefinition where
  show v = genericShow v

derive instance fieldsDefinitionEq ∷ Eq FieldsDefinition

_FieldsDefinition ∷
  Tuple
    ( (List FieldDefinition) → FieldsDefinition
    )
    ( FieldsDefinition →
      Maybe (List FieldDefinition)
    )
_FieldsDefinition =
  Tuple FieldsDefinition
    ( case _ of
        FieldsDefinition a → Just a
    )

data FieldsDefinition
  = FieldsDefinition (List FieldDefinition)

derive instance fieldDefinitionGeneric ∷ Generic FieldDefinition _

instance fieldDefinitionShow ∷ Show FieldDefinition where
  show v = genericShow v

derive instance fieldDefinitionEq ∷ Eq FieldDefinition

_FieldDefinition ∷
  Tuple
    ( { description ∷ (Maybe String), name ∷ String, argumentsDefinition ∷ (Maybe ArgumentsDefinition), type ∷ Type, directives ∷ (Maybe Directives) } → FieldDefinition
    )
    ( FieldDefinition →
      Maybe { description ∷ (Maybe String), name ∷ String, argumentsDefinition ∷ (Maybe ArgumentsDefinition), type ∷ Type, directives ∷ (Maybe Directives) }
    )
_FieldDefinition =
  Tuple FieldDefinition
    ( case _ of
        FieldDefinition a → Just a
    )

data FieldDefinition
  = FieldDefinition { description ∷ (Maybe String), name ∷ String, argumentsDefinition ∷ (Maybe ArgumentsDefinition), type ∷ Type, directives ∷ (Maybe Directives) }

derive instance argumentsDefinitionGeneric ∷ Generic ArgumentsDefinition _

instance argumentsDefinitionShow ∷ Show ArgumentsDefinition where
  show v = genericShow v

derive instance argumentsDefinitionEq ∷ Eq ArgumentsDefinition

_ArgumentsDefinition ∷
  Tuple
    ( (List InputValueDefinition) → ArgumentsDefinition
    )
    ( ArgumentsDefinition →
      Maybe (List InputValueDefinition)
    )
_ArgumentsDefinition =
  Tuple ArgumentsDefinition
    ( case _ of
        ArgumentsDefinition a → Just a
    )

data ArgumentsDefinition
  = ArgumentsDefinition (List InputValueDefinition)

derive instance inputValueDefinitionGeneric ∷ Generic InputValueDefinition _

instance inputValueDefinitionShow ∷ Show InputValueDefinition where
  show v = genericShow v

derive instance inputValueDefinitionEq ∷ Eq InputValueDefinition

_InputValueDefinition ∷
  Tuple
    ( { description ∷ (Maybe String), name ∷ String, type ∷ Type, defaultValue ∷ (Maybe DefaultValue), directives ∷ (Maybe Directives) } → InputValueDefinition
    )
    ( InputValueDefinition →
      Maybe { description ∷ (Maybe String), name ∷ String, type ∷ Type, defaultValue ∷ (Maybe DefaultValue), directives ∷ (Maybe Directives) }
    )
_InputValueDefinition =
  Tuple InputValueDefinition
    ( case _ of
        InputValueDefinition a → Just a
    )

data InputValueDefinition
  = InputValueDefinition { description ∷ (Maybe String), name ∷ String, type ∷ Type, defaultValue ∷ (Maybe DefaultValue), directives ∷ (Maybe Directives) }

derive instance interfaceTypeDefinitionGeneric ∷ Generic InterfaceTypeDefinition _

instance interfaceTypeDefinitionShow ∷ Show InterfaceTypeDefinition where
  show v = genericShow v

derive instance interfaceTypeDefinitionEq ∷ Eq InterfaceTypeDefinition

_InterfaceTypeDefinition ∷
  Tuple
    ( { description ∷ (Maybe String), name ∷ String, directives ∷ (Maybe Directives), fieldsDefinition ∷ (Maybe FieldsDefinition) } → InterfaceTypeDefinition
    )
    ( InterfaceTypeDefinition →
      Maybe { description ∷ (Maybe String), name ∷ String, directives ∷ (Maybe Directives), fieldsDefinition ∷ (Maybe FieldsDefinition) }
    )
_InterfaceTypeDefinition =
  Tuple InterfaceTypeDefinition
    ( case _ of
        InterfaceTypeDefinition a → Just a
    )

data InterfaceTypeDefinition
  = InterfaceTypeDefinition { description ∷ (Maybe String), name ∷ String, directives ∷ (Maybe Directives), fieldsDefinition ∷ (Maybe FieldsDefinition) }

derive instance interfaceTypeExtensionGeneric ∷ Generic InterfaceTypeExtension _

instance interfaceTypeExtensionShow ∷ Show InterfaceTypeExtension where
  show v = genericShow v

derive instance interfaceTypeExtensionEq ∷ Eq InterfaceTypeExtension

_InterfaceTypeExtension_With_FieldsDefinition ∷
  Tuple
    ( { name ∷ String, directives ∷ (Maybe Directives), fieldsDefinition ∷ FieldsDefinition } → InterfaceTypeExtension
    )
    ( InterfaceTypeExtension →
      Maybe { name ∷ String, directives ∷ (Maybe Directives), fieldsDefinition ∷ FieldsDefinition }
    )
_InterfaceTypeExtension_With_FieldsDefinition =
  Tuple InterfaceTypeExtension_With_FieldsDefinition
    ( case _ of
        InterfaceTypeExtension_With_FieldsDefinition a → Just a
        _ → Nothing
    )

_InterfaceTypeExtension_With_Directives ∷
  Tuple
    ( { name ∷ String, directives ∷ Directives } → InterfaceTypeExtension
    )
    ( InterfaceTypeExtension →
      Maybe { name ∷ String, directives ∷ Directives }
    )
_InterfaceTypeExtension_With_Directives =
  Tuple InterfaceTypeExtension_With_Directives
    ( case _ of
        InterfaceTypeExtension_With_Directives a → Just a
        _ → Nothing
    )

data InterfaceTypeExtension
  = InterfaceTypeExtension_With_FieldsDefinition { name ∷ String, directives ∷ (Maybe Directives), fieldsDefinition ∷ FieldsDefinition }
  | InterfaceTypeExtension_With_Directives { name ∷ String, directives ∷ Directives }

derive instance unionTypeDefinitionGeneric ∷ Generic UnionTypeDefinition _

instance unionTypeDefinitionShow ∷ Show UnionTypeDefinition where
  show v = genericShow v

derive instance unionTypeDefinitionEq ∷ Eq UnionTypeDefinition

_UnionTypeDefinition ∷
  Tuple
    ( { description ∷ Maybe String, name ∷ String, directives ∷ (Maybe Directives), unionMemberTypes ∷ (Maybe UnionMemberTypes) } → UnionTypeDefinition
    )
    ( UnionTypeDefinition →
      Maybe { description ∷ Maybe String, name ∷ String, directives ∷ (Maybe Directives), unionMemberTypes ∷ (Maybe UnionMemberTypes) }
    )
_UnionTypeDefinition =
  Tuple UnionTypeDefinition
    ( case _ of
        UnionTypeDefinition a → Just a
    )

data UnionTypeDefinition
  = UnionTypeDefinition { description ∷ Maybe String, name ∷ String, directives ∷ (Maybe Directives), unionMemberTypes ∷ (Maybe UnionMemberTypes) }

derive instance unionMemberTypesGeneric ∷ Generic UnionMemberTypes _

instance unionMemberTypesShow ∷ Show UnionMemberTypes where
  show v = genericShow v

derive instance unionMemberTypesEq ∷ Eq UnionMemberTypes

_UnionMemberTypes ∷
  Tuple
    ( (List NamedType) → UnionMemberTypes
    )
    ( UnionMemberTypes →
      Maybe (List NamedType)
    )
_UnionMemberTypes =
  Tuple UnionMemberTypes
    ( case _ of
        UnionMemberTypes a → Just a
    )

data UnionMemberTypes
  = UnionMemberTypes (List NamedType)

derive instance unionTypeExtensionGeneric ∷ Generic UnionTypeExtension _

instance unionTypeExtensionShow ∷ Show UnionTypeExtension where
  show v = genericShow v

derive instance unionTypeExtensionEq ∷ Eq UnionTypeExtension

_UnionTypeExtension_With_UnionMemberTypes ∷
  Tuple
    ( { name ∷ String, directives ∷ (Maybe Directives), unionMemberTypes ∷ UnionMemberTypes } → UnionTypeExtension
    )
    ( UnionTypeExtension →
      Maybe { name ∷ String, directives ∷ (Maybe Directives), unionMemberTypes ∷ UnionMemberTypes }
    )
_UnionTypeExtension_With_UnionMemberTypes =
  Tuple UnionTypeExtension_With_UnionMemberTypes
    ( case _ of
        UnionTypeExtension_With_UnionMemberTypes a → Just a
        _ → Nothing
    )

_UnionTypeExtension_With_Directives ∷
  Tuple
    ( { name ∷ String, directives ∷ Directives } → UnionTypeExtension
    )
    ( UnionTypeExtension →
      Maybe { name ∷ String, directives ∷ Directives }
    )
_UnionTypeExtension_With_Directives =
  Tuple UnionTypeExtension_With_Directives
    ( case _ of
        UnionTypeExtension_With_Directives a → Just a
        _ → Nothing
    )

data UnionTypeExtension
  = UnionTypeExtension_With_UnionMemberTypes { name ∷ String, directives ∷ (Maybe Directives), unionMemberTypes ∷ UnionMemberTypes }
  | UnionTypeExtension_With_Directives { name ∷ String, directives ∷ Directives }

derive instance enumTypeDefinitionGeneric ∷ Generic EnumTypeDefinition _

instance enumTypeDefinitionShow ∷ Show EnumTypeDefinition where
  show v = genericShow v

derive instance enumTypeDefinitionEq ∷ Eq EnumTypeDefinition

_EnumTypeDefinition ∷
  Tuple
    ( { description ∷ (Maybe String), name ∷ String, directives ∷ (Maybe Directives), enumValuesDefinition ∷ (Maybe EnumValuesDefinition) } → EnumTypeDefinition
    )
    ( EnumTypeDefinition →
      Maybe { description ∷ (Maybe String), name ∷ String, directives ∷ (Maybe Directives), enumValuesDefinition ∷ (Maybe EnumValuesDefinition) }
    )
_EnumTypeDefinition =
  Tuple EnumTypeDefinition
    ( case _ of
        EnumTypeDefinition a → Just a
    )

data EnumTypeDefinition
  = EnumTypeDefinition { description ∷ (Maybe String), name ∷ String, directives ∷ (Maybe Directives), enumValuesDefinition ∷ (Maybe EnumValuesDefinition) }

derive instance enumValuesDefinitionGeneric ∷ Generic EnumValuesDefinition _

instance enumValuesDefinitionShow ∷ Show EnumValuesDefinition where
  show v = genericShow v

derive instance enumValuesDefinitionEq ∷ Eq EnumValuesDefinition

_EnumValuesDefinition ∷
  Tuple
    ( (List EnumValueDefinition) → EnumValuesDefinition
    )
    ( EnumValuesDefinition →
      Maybe (List EnumValueDefinition)
    )
_EnumValuesDefinition =
  Tuple EnumValuesDefinition
    ( case _ of
        EnumValuesDefinition a → Just a
    )

data EnumValuesDefinition
  = EnumValuesDefinition (List EnumValueDefinition)

derive instance enumValueDefinitionGeneric ∷ Generic EnumValueDefinition _

instance enumValueDefinitionShow ∷ Show EnumValueDefinition where
  show v = genericShow v

derive instance enumValueDefinitionEq ∷ Eq EnumValueDefinition

_EnumValueDefinition ∷
  Tuple
    ( { description ∷ (Maybe String), enumValue ∷ EnumValue, directives ∷ (Maybe Directives) } → EnumValueDefinition
    )
    ( EnumValueDefinition →
      Maybe { description ∷ (Maybe String), enumValue ∷ EnumValue, directives ∷ (Maybe Directives) }
    )
_EnumValueDefinition =
  Tuple EnumValueDefinition
    ( case _ of
        EnumValueDefinition a → Just a
    )

data EnumValueDefinition
  = EnumValueDefinition { description ∷ (Maybe String), enumValue ∷ EnumValue, directives ∷ (Maybe Directives) }

derive instance enumTypeExtensionGeneric ∷ Generic EnumTypeExtension _

instance enumTypeExtensionShow ∷ Show EnumTypeExtension where
  show v = genericShow v

derive instance enumTypeExtensionEq ∷ Eq EnumTypeExtension

_EnumTypeExtension_With_EnumValuesDefinition ∷
  Tuple
    ( { name ∷ String, directives ∷ (Maybe Directives), enumValuesDefinition ∷ EnumValuesDefinition } → EnumTypeExtension
    )
    ( EnumTypeExtension →
      Maybe { name ∷ String, directives ∷ (Maybe Directives), enumValuesDefinition ∷ EnumValuesDefinition }
    )
_EnumTypeExtension_With_EnumValuesDefinition =
  Tuple EnumTypeExtension_With_EnumValuesDefinition
    ( case _ of
        EnumTypeExtension_With_EnumValuesDefinition a → Just a
        _ → Nothing
    )

_EnumTypeExtension_With_Directives ∷
  Tuple
    ( { name ∷ String, directives ∷ Directives } → EnumTypeExtension
    )
    ( EnumTypeExtension →
      Maybe { name ∷ String, directives ∷ Directives }
    )
_EnumTypeExtension_With_Directives =
  Tuple EnumTypeExtension_With_Directives
    ( case _ of
        EnumTypeExtension_With_Directives a → Just a
        _ → Nothing
    )

data EnumTypeExtension
  = EnumTypeExtension_With_EnumValuesDefinition { name ∷ String, directives ∷ (Maybe Directives), enumValuesDefinition ∷ EnumValuesDefinition }
  | EnumTypeExtension_With_Directives { name ∷ String, directives ∷ Directives }

derive instance inputObjectTypeDefinitionGeneric ∷ Generic InputObjectTypeDefinition _

instance inputObjectTypeDefinitionShow ∷ Show InputObjectTypeDefinition where
  show v = genericShow v

derive instance inputObjectTypeDefinitionEq ∷ Eq InputObjectTypeDefinition

_InputObjectTypeDefinition ∷
  Tuple
    ( { description ∷ (Maybe String), name ∷ String, directives ∷ (Maybe Directives), inputFieldsDefinition ∷ (Maybe InputFieldsDefinition) } → InputObjectTypeDefinition
    )
    ( InputObjectTypeDefinition →
      Maybe { description ∷ (Maybe String), name ∷ String, directives ∷ (Maybe Directives), inputFieldsDefinition ∷ (Maybe InputFieldsDefinition) }
    )
_InputObjectTypeDefinition =
  Tuple InputObjectTypeDefinition
    ( case _ of
        InputObjectTypeDefinition a → Just a
    )

data InputObjectTypeDefinition
  = InputObjectTypeDefinition { description ∷ (Maybe String), name ∷ String, directives ∷ (Maybe Directives), inputFieldsDefinition ∷ (Maybe InputFieldsDefinition) }

derive instance inputFieldsDefinitionGeneric ∷ Generic InputFieldsDefinition _

instance inputFieldsDefinitionShow ∷ Show InputFieldsDefinition where
  show v = genericShow v

derive instance inputFieldsDefinitionEq ∷ Eq InputFieldsDefinition

_InputFieldsDefinition ∷
  Tuple
    ( (List InputValueDefinition) → InputFieldsDefinition
    )
    ( InputFieldsDefinition →
      Maybe (List InputValueDefinition)
    )
_InputFieldsDefinition =
  Tuple InputFieldsDefinition
    ( case _ of
        InputFieldsDefinition a → Just a
    )

data InputFieldsDefinition
  = InputFieldsDefinition (List InputValueDefinition)

derive instance inputObjectTypeExtensionGeneric ∷ Generic InputObjectTypeExtension _

instance inputObjectTypeExtensionShow ∷ Show InputObjectTypeExtension where
  show v = genericShow v

derive instance inputObjectTypeExtensionEq ∷ Eq InputObjectTypeExtension

_InputObjectTypeExtension_With_InputFieldsDefinition ∷
  Tuple
    ( { name ∷ String, directives ∷ (Maybe Directives), inputFieldsDefinition ∷ InputFieldsDefinition } → InputObjectTypeExtension
    )
    ( InputObjectTypeExtension →
      Maybe { name ∷ String, directives ∷ (Maybe Directives), inputFieldsDefinition ∷ InputFieldsDefinition }
    )
_InputObjectTypeExtension_With_InputFieldsDefinition =
  Tuple InputObjectTypeExtension_With_InputFieldsDefinition
    ( case _ of
        InputObjectTypeExtension_With_InputFieldsDefinition a → Just a
        _ → Nothing
    )

_InputObjectTypeExtension_With_Directives ∷
  Tuple
    ( { name ∷ String, directives ∷ Directives } → InputObjectTypeExtension
    )
    ( InputObjectTypeExtension →
      Maybe { name ∷ String, directives ∷ Directives }
    )
_InputObjectTypeExtension_With_Directives =
  Tuple InputObjectTypeExtension_With_Directives
    ( case _ of
        InputObjectTypeExtension_With_Directives a → Just a
        _ → Nothing
    )

data InputObjectTypeExtension
  = InputObjectTypeExtension_With_InputFieldsDefinition { name ∷ String, directives ∷ (Maybe Directives), inputFieldsDefinition ∷ InputFieldsDefinition }
  | InputObjectTypeExtension_With_Directives { name ∷ String, directives ∷ Directives }

derive instance directiveDefinitionGeneric ∷ Generic DirectiveDefinition _

instance directiveDefinitionShow ∷ Show DirectiveDefinition where
  show v = genericShow v

derive instance directiveDefinitionEq ∷ Eq DirectiveDefinition

_DirectiveDefinition ∷
  Tuple
    ( { description ∷ (Maybe String), name ∷ String, argumentsDefinition ∷ (Maybe ArgumentsDefinition), directiveLocations ∷ DirectiveLocations } → DirectiveDefinition
    )
    ( DirectiveDefinition →
      Maybe { description ∷ (Maybe String), name ∷ String, argumentsDefinition ∷ (Maybe ArgumentsDefinition), directiveLocations ∷ DirectiveLocations }
    )
_DirectiveDefinition =
  Tuple DirectiveDefinition
    ( case _ of
        DirectiveDefinition a → Just a
    )

data DirectiveDefinition
  = DirectiveDefinition { description ∷ (Maybe String), name ∷ String, argumentsDefinition ∷ (Maybe ArgumentsDefinition), directiveLocations ∷ DirectiveLocations }

derive instance directiveLocationsGeneric ∷ Generic DirectiveLocations _

instance directiveLocationsShow ∷ Show DirectiveLocations where
  show v = genericShow v

derive instance directiveLocationsEq ∷ Eq DirectiveLocations

_DirectiveLocations ∷
  Tuple
    ( (List DirectiveLocation) → DirectiveLocations
    )
    ( DirectiveLocations →
      Maybe (List DirectiveLocation)
    )
_DirectiveLocations =
  Tuple DirectiveLocations
    ( case _ of
        DirectiveLocations a → Just a
    )

data DirectiveLocations
  = DirectiveLocations (List DirectiveLocation)

derive instance directiveLocationGeneric ∷ Generic DirectiveLocation _

instance directiveLocationShow ∷ Show DirectiveLocation where
  show v = genericShow v

derive instance directiveLocationEq ∷ Eq DirectiveLocation

_DirectiveLocation_ExecutableDirectiveLocation ∷
  Tuple
    ( ExecutableDirectiveLocation → DirectiveLocation
    )
    ( DirectiveLocation →
      Maybe ExecutableDirectiveLocation
    )
_DirectiveLocation_ExecutableDirectiveLocation =
  Tuple DirectiveLocation_ExecutableDirectiveLocation
    ( case _ of
        DirectiveLocation_ExecutableDirectiveLocation a → Just a
        _ → Nothing
    )

_DirectiveLocation_TypeSystemDirectiveLocation ∷
  Tuple
    ( TypeSystemDirectiveLocation → DirectiveLocation
    )
    ( DirectiveLocation →
      Maybe TypeSystemDirectiveLocation
    )
_DirectiveLocation_TypeSystemDirectiveLocation =
  Tuple DirectiveLocation_TypeSystemDirectiveLocation
    ( case _ of
        DirectiveLocation_TypeSystemDirectiveLocation a → Just a
        _ → Nothing
    )

data DirectiveLocation
  = DirectiveLocation_ExecutableDirectiveLocation ExecutableDirectiveLocation
  | DirectiveLocation_TypeSystemDirectiveLocation TypeSystemDirectiveLocation

derive instance executableDirectiveLocationGeneric ∷ Generic ExecutableDirectiveLocation _

instance executableDirectiveLocationShow ∷ Show ExecutableDirectiveLocation where
  show v = genericShow v

derive instance executableDirectiveLocationEq ∷ Eq ExecutableDirectiveLocation

_QUERY ∷
  Tuple
    ( Unit → ExecutableDirectiveLocation
    )
    ( ExecutableDirectiveLocation →
      Maybe Unit
    )
_QUERY =
  Tuple (\_ → QUERY)
    ( case _ of
        QUERY → Just unit
        _ → Nothing
    )

_MUTATION ∷
  Tuple
    ( Unit → ExecutableDirectiveLocation
    )
    ( ExecutableDirectiveLocation →
      Maybe Unit
    )
_MUTATION =
  Tuple (\_ → MUTATION)
    ( case _ of
        MUTATION → Just unit
        _ → Nothing
    )

_SUBSCRIPTION ∷
  Tuple
    ( Unit → ExecutableDirectiveLocation
    )
    ( ExecutableDirectiveLocation →
      Maybe Unit
    )
_SUBSCRIPTION =
  Tuple (\_ → SUBSCRIPTION)
    ( case _ of
        SUBSCRIPTION → Just unit
        _ → Nothing
    )

_FIELD ∷
  Tuple
    ( Unit → ExecutableDirectiveLocation
    )
    ( ExecutableDirectiveLocation →
      Maybe Unit
    )
_FIELD =
  Tuple (\_ → FIELD)
    ( case _ of
        FIELD → Just unit
        _ → Nothing
    )

_FRAGMENT_DEFINITION ∷
  Tuple
    ( Unit → ExecutableDirectiveLocation
    )
    ( ExecutableDirectiveLocation →
      Maybe Unit
    )
_FRAGMENT_DEFINITION =
  Tuple (\_ → FRAGMENT_DEFINITION)
    ( case _ of
        FRAGMENT_DEFINITION → Just unit
        _ → Nothing
    )

_FRAGMENT_SPREAD ∷
  Tuple
    ( Unit → ExecutableDirectiveLocation
    )
    ( ExecutableDirectiveLocation →
      Maybe Unit
    )
_FRAGMENT_SPREAD =
  Tuple (\_ → FRAGMENT_SPREAD)
    ( case _ of
        FRAGMENT_SPREAD → Just unit
        _ → Nothing
    )

_INLINE_FRAGMENT ∷
  Tuple
    ( Unit → ExecutableDirectiveLocation
    )
    ( ExecutableDirectiveLocation →
      Maybe Unit
    )
_INLINE_FRAGMENT =
  Tuple (\_ → INLINE_FRAGMENT)
    ( case _ of
        INLINE_FRAGMENT → Just unit
        _ → Nothing
    )

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

_SCHEMA ∷
  Tuple
    ( Unit → TypeSystemDirectiveLocation
    )
    ( TypeSystemDirectiveLocation →
      Maybe Unit
    )
_SCHEMA =
  Tuple (\_ → SCHEMA)
    ( case _ of
        SCHEMA → Just unit
        _ → Nothing
    )

_SCALAR ∷
  Tuple
    ( Unit → TypeSystemDirectiveLocation
    )
    ( TypeSystemDirectiveLocation →
      Maybe Unit
    )
_SCALAR =
  Tuple (\_ → SCALAR)
    ( case _ of
        SCALAR → Just unit
        _ → Nothing
    )

_OBJECT ∷
  Tuple
    ( Unit → TypeSystemDirectiveLocation
    )
    ( TypeSystemDirectiveLocation →
      Maybe Unit
    )
_OBJECT =
  Tuple (\_ → OBJECT)
    ( case _ of
        OBJECT → Just unit
        _ → Nothing
    )

_FIELD_DEFINITION ∷
  Tuple
    ( Unit → TypeSystemDirectiveLocation
    )
    ( TypeSystemDirectiveLocation →
      Maybe Unit
    )
_FIELD_DEFINITION =
  Tuple (\_ → FIELD_DEFINITION)
    ( case _ of
        FIELD_DEFINITION → Just unit
        _ → Nothing
    )

_ARGUMENT_DEFINITION ∷
  Tuple
    ( Unit → TypeSystemDirectiveLocation
    )
    ( TypeSystemDirectiveLocation →
      Maybe Unit
    )
_ARGUMENT_DEFINITION =
  Tuple (\_ → ARGUMENT_DEFINITION)
    ( case _ of
        ARGUMENT_DEFINITION → Just unit
        _ → Nothing
    )

_INTERFACE ∷
  Tuple
    ( Unit → TypeSystemDirectiveLocation
    )
    ( TypeSystemDirectiveLocation →
      Maybe Unit
    )
_INTERFACE =
  Tuple (\_ → INTERFACE)
    ( case _ of
        INTERFACE → Just unit
        _ → Nothing
    )

_UNION ∷
  Tuple
    ( Unit → TypeSystemDirectiveLocation
    )
    ( TypeSystemDirectiveLocation →
      Maybe Unit
    )
_UNION =
  Tuple (\_ → UNION)
    ( case _ of
        UNION → Just unit
        _ → Nothing
    )

_ENUM ∷
  Tuple
    ( Unit → TypeSystemDirectiveLocation
    )
    ( TypeSystemDirectiveLocation →
      Maybe Unit
    )
_ENUM =
  Tuple (\_ → ENUM)
    ( case _ of
        ENUM → Just unit
        _ → Nothing
    )

_ENUM_VALUE ∷
  Tuple
    ( Unit → TypeSystemDirectiveLocation
    )
    ( TypeSystemDirectiveLocation →
      Maybe Unit
    )
_ENUM_VALUE =
  Tuple (\_ → ENUM_VALUE)
    ( case _ of
        ENUM_VALUE → Just unit
        _ → Nothing
    )

_INPUT_OBJECT ∷
  Tuple
    ( Unit → TypeSystemDirectiveLocation
    )
    ( TypeSystemDirectiveLocation →
      Maybe Unit
    )
_INPUT_OBJECT =
  Tuple (\_ → INPUT_OBJECT)
    ( case _ of
        INPUT_OBJECT → Just unit
        _ → Nothing
    )

_INPUT_FIELD_DEFINITION ∷
  Tuple
    ( Unit → TypeSystemDirectiveLocation
    )
    ( TypeSystemDirectiveLocation →
      Maybe Unit
    )
_INPUT_FIELD_DEFINITION =
  Tuple (\_ → INPUT_FIELD_DEFINITION)
    ( case _ of
        INPUT_FIELD_DEFINITION → Just unit
        _ → Nothing
    )

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
