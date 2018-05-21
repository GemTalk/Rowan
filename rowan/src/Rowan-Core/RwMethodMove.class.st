"
Represents a move of a method from a class definition to a class extension, or from a class extension to a class definition, or between two extensions, of the same class. This should always also result in a change of package, since a class should not be defined and extended in the same package, nor extended twice in the same package.
"
Class {
	#name : 'RwMethodMove',
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
	#category : 'Rowan-Core'
}

{ #category : 'unknown' }
RwMethodMove class >> methodBefore: beforeMethodDefinition methodAfter: afterMethodDefinition classOrExtensionBefore: beforeClassDefinitionOrExtension classOrExtensionAfter: afterClassDefinitionOrExtension packageBefore: beforePackageDefinition packageAfter: afterPackageDefinition isMeta: isMeta [

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
RwMethodMove >> classOrExtensionAfter [
	^classOrExtensionAfter

]

{ #category : 'Updating' }
RwMethodMove >> classOrExtensionAfter: newValue [
	classOrExtensionAfter := newValue

]

{ #category : 'Accessing' }
RwMethodMove >> classOrExtensionBefore [
	^classOrExtensionBefore

]

{ #category : 'Updating' }
RwMethodMove >> classOrExtensionBefore: newValue [
	classOrExtensionBefore := newValue

]

{ #category : 'Accessing' }
RwMethodMove >> isMeta [
	^isMeta

]

{ #category : 'Updating' }
RwMethodMove >> isMeta: newValue [
	isMeta := newValue

]

{ #category : 'Accessing' }
RwMethodMove >> methodAfter [
	^methodAfter

]

{ #category : 'Updating' }
RwMethodMove >> methodAfter: newValue [
	methodAfter := newValue

]

{ #category : 'Accessing' }
RwMethodMove >> methodBefore [
	^methodBefore

]

{ #category : 'Updating' }
RwMethodMove >> methodBefore: newValue [
	methodBefore := newValue

]

{ #category : 'Accessing' }
RwMethodMove >> packageAfter [
	^packageAfter

]

{ #category : 'Updating' }
RwMethodMove >> packageAfter: newValue [
	packageAfter := newValue

]

{ #category : 'Accessing' }
RwMethodMove >> packageBefore [
	^packageBefore

]

{ #category : 'Updating' }
RwMethodMove >> packageBefore: newValue [
	packageBefore := newValue

]
