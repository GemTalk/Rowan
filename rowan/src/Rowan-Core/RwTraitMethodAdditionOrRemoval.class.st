Class {
	#name : 'RwTraitMethodAdditionOrRemoval',
	#superclass : 'Object',
	#instVars : [
		'projectDefinition',
		'packageDefinition',
		'traitDefinition',
		'methodKey',
		'isMeta',
		'methodsModification'
	],
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwTraitMethodAdditionOrRemoval class >> packageDefinition: aPackageDefinition traitDefinition: aTraitDefinition methodKey: aMethodKey isMeta: isMeta methodsModification: aMethodsModification [

	^(self new)
		packageDefinition: aPackageDefinition;
		traitDefinition: aTraitDefinition;
		methodKey: aMethodKey;
		isMeta: isMeta;
		methodsModification: aMethodsModification;
		yourself
]

{ #category : 'instance creation' }
RwTraitMethodAdditionOrRemoval class >> projectDefinition: aProjectDefinition packageDefinition: aPackageDefinition traitDefinition: aTraitDefinition methodKey: aMethodKey isMeta: isMeta methodsModification: aMethodsModification [

	^(self new)
		projectDefinition: aProjectDefinition;
		packageDefinition: aPackageDefinition;
		traitDefinition: aTraitDefinition;
		methodKey: aMethodKey;
		isMeta: isMeta;
		methodsModification: aMethodsModification;
		yourself
]

{ #category : 'Accessing' }
RwTraitMethodAdditionOrRemoval >> isMeta [
	^isMeta

]

{ #category : 'Updating' }
RwTraitMethodAdditionOrRemoval >> isMeta: newValue [
	isMeta := newValue

]

{ #category : 'Accessing' }
RwTraitMethodAdditionOrRemoval >> methodKey [
	^methodKey

]

{ #category : 'Updating' }
RwTraitMethodAdditionOrRemoval >> methodKey: newValue [
	methodKey := newValue

]

{ #category : 'Accessing' }
RwTraitMethodAdditionOrRemoval >> methodsModification [
	^methodsModification

]

{ #category : 'Updating' }
RwTraitMethodAdditionOrRemoval >> methodsModification: newValue [
	methodsModification := newValue

]

{ #category : 'Accessing' }
RwTraitMethodAdditionOrRemoval >> packageDefinition [
	^packageDefinition

]

{ #category : 'Updating' }
RwTraitMethodAdditionOrRemoval >> packageDefinition: newValue [
	packageDefinition := newValue

]

{ #category : 'Accessing' }
RwTraitMethodAdditionOrRemoval >> projectDefinition [
	^ projectDefinition

]

{ #category : 'Updating' }
RwTraitMethodAdditionOrRemoval >> projectDefinition: newValue [
	projectDefinition := newValue

]

{ #category : 'accessing' }
RwTraitMethodAdditionOrRemoval >> traitDefinition [
	^traitDefinition
]

{ #category : 'accessing' }
RwTraitMethodAdditionOrRemoval >> traitDefinition: object [
	traitDefinition := object
]
