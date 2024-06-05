Class {
	#name : 'RwTraitAdditionOrRemoval',
	#superclass : 'Object',
	#instVars : [
		'projectDefinition',
		'packageDefinition',
		'traitKey',
		'traitsModification'
	],
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwTraitAdditionOrRemoval class >> packageDefinition: aPackageDefinition traitKey: aTraitKey traitsModification: aTraitsModification [
	^ self new
		packageDefinition: aPackageDefinition;
		traitKey: aTraitKey;
		traitsModification: aTraitsModification;
		yourself
]

{ #category : 'instance creation' }
RwTraitAdditionOrRemoval class >> projectDefinition: aProjectDefinition packageDefinition: aPackageDefinition traitKey: aTraitKey traitsModification: aTraitsModification [
	^ self new
		projectDefinition: aProjectDefinition;
		packageDefinition: aPackageDefinition;
		traitKey: aTraitKey;
		traitsModification: aTraitsModification;
		yourself
]

{ #category : 'Accessing' }
RwTraitAdditionOrRemoval >> packageDefinition [
	^packageDefinition

]

{ #category : 'Updating' }
RwTraitAdditionOrRemoval >> packageDefinition: newValue [
	packageDefinition := newValue

]

{ #category : 'Accessing' }
RwTraitAdditionOrRemoval >> projectDefinition [
	^projectDefinition

]

{ #category : 'Updating' }
RwTraitAdditionOrRemoval >> projectDefinition: newValue [
	projectDefinition := newValue

]

{ #category : 'accessing' }
RwTraitAdditionOrRemoval >> traitKey [
	^traitKey
]

{ #category : 'accessing' }
RwTraitAdditionOrRemoval >> traitKey: object [
	traitKey := object
]

{ #category : 'accessing' }
RwTraitAdditionOrRemoval >> traitsModification [
	^traitsModification
]

{ #category : 'accessing' }
RwTraitAdditionOrRemoval >> traitsModification: object [
	traitsModification := object
]
