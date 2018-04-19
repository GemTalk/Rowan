Class {
	#name : 'TonelReaderTest',
	#superclass : 'TonelAbstractTest',
	#instVars : [
		'directory'
	],
	#category : 'Tonel-Tests'
}

{ #category : 'Testing' }
TonelReaderTest class >> isAbstract [
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

	^self sunitName = #TonelReaderTest

]

{ #category : 'tests' }
TonelReaderTest >> assertClassDefinition: a and: b [
	self assert: a className equals: b className.
	self assert: a superclassName equals: b superclassName.
	self assert: a traitCompositionString equals: b traitCompositionString.
	self assert: a classTraitCompositionString equals: b classTraitCompositionString.
	self assert: a category equals: b category.	
	self assert: a instVarNames equals: b instVarNames.
	self assert: a classInstVarNames equals: b classInstVarNames.
	self assert: a classVarNames equals: b classVarNames.
	self assert: a poolDictionaries equals: b poolDictionaries.
	self assert: a type equals: b type.
	self assert: a comment equals: b comment.

]

{ #category : 'tests' }
TonelReaderTest >> assertDefinition: a and: b [
	a isOrganizationDefinition ifTrue: [ ^ self assertOrganisationDefinition: a and: b ].
	a isClassDefinition ifTrue: [ ^ self assertClassDefinition: a and: b ].
	a isMethodDefinition ifTrue: [ ^ self assertMethodDefinition: a and: b ].

]

{ #category : 'tests' }
TonelReaderTest >> assertMethodDefinition: a and: b [
	self assert: a className equals: b className.
	self assert: a selector equals: b selector.
	self assert: a protocol equals: b protocol.
	self assert: a source asByteArray equals: b source asByteArray.
	self assert: a classIsMeta equals: b classIsMeta

]

{ #category : 'tests' }
TonelReaderTest >> assertOrganisationDefinition: a and: b [
	self assert: a categories equals: b categories

]

{ #category : 'mocks' }
TonelReaderTest >> mockSnapshot [

	self subclassResponsibility

]

{ #category : 'tests' }
TonelReaderTest >> testLoadDefinitions [
  | snapshot reader |
  snapshot := self mockSnapshot.
  reader := self createReaderFor: snapshot fileName: 'MonticelloMocks'.
  reader loadDefinitions.
  self assert: reader definitions size equals: snapshot definitions size.
  reader definitions sorted
    with: snapshot definitions sorted
    do: [ :a :b | self assertDefinition: a and: b ]

]
