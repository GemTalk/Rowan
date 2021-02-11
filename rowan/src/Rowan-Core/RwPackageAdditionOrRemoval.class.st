Class {
	#name : 'RwPackageAdditionOrRemoval',
	#superclass : 'Object',
	#instVars : [
		'projectDefinition',
		'packageDefinition',
		'packageKey',
		'packagesModification'
	],
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwPackageAdditionOrRemoval class >> projectDefinition: aProjectDefinition packageDefinition: aPackageDefinition packageKey: aPackageKey packagesModification: aPackagesModification [

	^(self new)
		projectDefinition: aProjectDefinition;
		packageDefinition: aPackageDefinition;
		packageKey: aPackageKey;
		packagesModification: aPackagesModification;
		yourself
]

{ #category : 'Accessing' }
RwPackageAdditionOrRemoval >> packageDefinition [
	^packageDefinition

]

{ #category : 'Updating' }
RwPackageAdditionOrRemoval >> packageDefinition: newValue [
	packageDefinition := newValue

]

{ #category : 'accessing' }
RwPackageAdditionOrRemoval >> packageKey [
	^packageKey
]

{ #category : 'accessing' }
RwPackageAdditionOrRemoval >> packageKey: object [
	packageKey := object
]

{ #category : 'accessing' }
RwPackageAdditionOrRemoval >> packagesModification [
	^packagesModification
]

{ #category : 'accessing' }
RwPackageAdditionOrRemoval >> packagesModification: object [
	packagesModification := object
]

{ #category : 'Accessing' }
RwPackageAdditionOrRemoval >> projectDefinition [
	^projectDefinition

]

{ #category : 'Updating' }
RwPackageAdditionOrRemoval >> projectDefinition: newValue [
	projectDefinition := newValue

]
