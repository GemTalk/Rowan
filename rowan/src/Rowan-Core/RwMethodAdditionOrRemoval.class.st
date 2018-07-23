"
No class-specific documentation for CypMethodAdditionOrRemoval, hierarchy is: 
Object
  CypMethodAdditionOrRemoval( packageDefinition classDefinitionOrExtension methodKey isMeta methodsModification)

"
Class {
	#name : 'RwMethodAdditionOrRemoval',
	#superclass : 'Object',
	#instVars : [
		'projectDefinition',
		'packageDefinition',
		'classDefinitionOrExtension',
		'methodKey',
		'isMeta',
		'methodsModification'
	],
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwMethodAdditionOrRemoval class >> packageDefinition: aPackageDefinition classDefinitionOrExtension: aClassDefinitionOrExtension methodKey: aMethodKey isMeta: isMeta methodsModification: aMethodsModification [

	^(self new)
		packageDefinition: aPackageDefinition;
		classDefinitionOrExtension: aClassDefinitionOrExtension;
		methodKey: aMethodKey;
		isMeta: isMeta;
		methodsModification: aMethodsModification;
		yourself
]

{ #category : 'instance creation' }
RwMethodAdditionOrRemoval class >> projectDefinition: aProjectDefinition packageDefinition: aPackageDefinition classDefinitionOrExtension: aClassDefinitionOrExtension methodKey: aMethodKey isMeta: isMeta methodsModification: aMethodsModification [

	^(self new)
		projectDefinition: aProjectDefinition;
		packageDefinition: aPackageDefinition;
		classDefinitionOrExtension: aClassDefinitionOrExtension;
		methodKey: aMethodKey;
		isMeta: isMeta;
		methodsModification: aMethodsModification;
		yourself

]

{ #category : 'Accessing' }
RwMethodAdditionOrRemoval >> classDefinitionOrExtension [
	^classDefinitionOrExtension

]

{ #category : 'Updating' }
RwMethodAdditionOrRemoval >> classDefinitionOrExtension: newValue [
	classDefinitionOrExtension := newValue

]

{ #category : 'Accessing' }
RwMethodAdditionOrRemoval >> isMeta [
	^isMeta

]

{ #category : 'Updating' }
RwMethodAdditionOrRemoval >> isMeta: newValue [
	isMeta := newValue

]

{ #category : 'Accessing' }
RwMethodAdditionOrRemoval >> methodKey [
	^methodKey

]

{ #category : 'Updating' }
RwMethodAdditionOrRemoval >> methodKey: newValue [
	methodKey := newValue

]

{ #category : 'Accessing' }
RwMethodAdditionOrRemoval >> methodsModification [
	^methodsModification

]

{ #category : 'Updating' }
RwMethodAdditionOrRemoval >> methodsModification: newValue [
	methodsModification := newValue

]

{ #category : 'Accessing' }
RwMethodAdditionOrRemoval >> packageDefinition [
	^packageDefinition

]

{ #category : 'Updating' }
RwMethodAdditionOrRemoval >> packageDefinition: newValue [
	packageDefinition := newValue

]

{ #category : 'Accessing' }
RwMethodAdditionOrRemoval >> projectDefinition [
	^ projectDefinition

]

{ #category : 'Updating' }
RwMethodAdditionOrRemoval >> projectDefinition: newValue [
	projectDefinition := newValue

]
