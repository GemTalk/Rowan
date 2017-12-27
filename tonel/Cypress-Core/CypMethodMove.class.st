"
Represents a move of a method from a class definition to a class extension, or from a class extension to a class definition, or between two extensions, of the same class. This should always also result in a change of package, since a class should not be defined and extended in the same package, nor extended twice in the same package.
"
Class {
	#name : 'CypMethodMove',
	#superclass : 'Object',
	#instVars : [
		'methodBefore',
		'methodAfter',
		'classOrExtensionBefore',
		'classOrExtensionAfter',
		'packageBefore',
		'packageAfter',
		'isMeta'
	],
	#category : 'Cypress-Core'
}

{ #category : 'unknown' }
CypMethodMove class >> methodBefore: beforeMethodDefinition methodAfter: afterMethodDefinition classOrExtensionBefore: beforeClassDefinitionOrExtension classOrExtensionAfter: afterClassDefinitionOrExtension packageBefore: beforePackageDefinition packageAfter: afterPackageDefinition isMeta: isMeta [

	^(self new)
		methodBefore: beforeMethodDefinition;
		methodAfter: afterMethodDefinition;
		classOrExtensionBefore: beforeClassDefinitionOrExtension;
		classOrExtensionAfter: afterClassDefinitionOrExtension;
		packageBefore: beforePackageDefinition;
		packageAfter: afterPackageDefinition;
		isMeta: isMeta;
		yourself
]

{ #category : 'Accessing' }
CypMethodMove >> classOrExtensionAfter [
	^classOrExtensionAfter

]

{ #category : 'Updating' }
CypMethodMove >> classOrExtensionAfter: newValue [
	classOrExtensionAfter := newValue

]

{ #category : 'Accessing' }
CypMethodMove >> classOrExtensionBefore [
	^classOrExtensionBefore

]

{ #category : 'Updating' }
CypMethodMove >> classOrExtensionBefore: newValue [
	classOrExtensionBefore := newValue

]

{ #category : 'Accessing' }
CypMethodMove >> isMeta [
	^isMeta

]

{ #category : 'Updating' }
CypMethodMove >> isMeta: newValue [
	isMeta := newValue

]

{ #category : 'Accessing' }
CypMethodMove >> methodAfter [
	^methodAfter

]

{ #category : 'Updating' }
CypMethodMove >> methodAfter: newValue [
	methodAfter := newValue

]

{ #category : 'Accessing' }
CypMethodMove >> methodBefore [
	^methodBefore

]

{ #category : 'Updating' }
CypMethodMove >> methodBefore: newValue [
	methodBefore := newValue

]

{ #category : 'Accessing' }
CypMethodMove >> packageAfter [
	^packageAfter

]

{ #category : 'Updating' }
CypMethodMove >> packageAfter: newValue [
	packageAfter := newValue

]

{ #category : 'Accessing' }
CypMethodMove >> packageBefore [
	^packageBefore

]

{ #category : 'Updating' }
CypMethodMove >> packageBefore: newValue [
	packageBefore := newValue

]
