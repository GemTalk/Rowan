"
No class-specific documentation for CypMethodAdditionOrRemoval, hierarchy is: 
Object
  CypMethodAdditionOrRemoval( packageDefinition classDefinitionOrExtension methodKey isMeta methodsModification)

"
Class {
	#name : 'CypMethodAdditionOrRemoval',
	#superclass : 'Object',
	#instVars : [
		'packageDefinition',
		'classDefinitionOrExtension',
		'methodKey',
		'isMeta',
		'methodsModification'
	],
	#category : 'Cypress-Core'
}

{ #category : 'instance creation' }
CypMethodAdditionOrRemoval class >> packageDefinition: aPackageDefinition classDefinitionOrExtension: aClassDefinitionOrExtension methodKey: aMethodKey isMeta: isMeta methodsModification: aMethodsModification [

	^(self new)
		packageDefinition: aPackageDefinition;
		classDefinitionOrExtension: aClassDefinitionOrExtension;
		methodKey: aMethodKey;
		isMeta: isMeta;
		methodsModification: aMethodsModification;
		yourself
]

{ #category : 'Accessing' }
CypMethodAdditionOrRemoval >> classDefinitionOrExtension [
	^classDefinitionOrExtension

]

{ #category : 'Updating' }
CypMethodAdditionOrRemoval >> classDefinitionOrExtension: newValue [
	classDefinitionOrExtension := newValue

]

{ #category : 'Accessing' }
CypMethodAdditionOrRemoval >> isMeta [
	^isMeta

]

{ #category : 'Updating' }
CypMethodAdditionOrRemoval >> isMeta: newValue [
	isMeta := newValue

]

{ #category : 'Accessing' }
CypMethodAdditionOrRemoval >> methodKey [
	^methodKey

]

{ #category : 'Updating' }
CypMethodAdditionOrRemoval >> methodKey: newValue [
	methodKey := newValue

]

{ #category : 'Accessing' }
CypMethodAdditionOrRemoval >> methodsModification [
	^methodsModification

]

{ #category : 'Updating' }
CypMethodAdditionOrRemoval >> methodsModification: newValue [
	methodsModification := newValue

]

{ #category : 'Accessing' }
CypMethodAdditionOrRemoval >> packageDefinition [
	^packageDefinition

]

{ #category : 'Updating' }
CypMethodAdditionOrRemoval >> packageDefinition: newValue [
	packageDefinition := newValue

]
