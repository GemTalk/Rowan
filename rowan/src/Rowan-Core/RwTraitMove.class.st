Class {
	#name : 'RwTraitMove',
	#superclass : 'RwMove',
	#instVars : [
		'traitBefore',
		'traitAfter',
		'classBefore',
		'classAfter'
	],
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwTraitMove class >> traitBefore: beforeTraitDefinition traitAfter: afterTraitDefinition packageBefore: beforePackageDefinition packageAfter: afterPackageDefinition [

	^(self new)
		traitBefore: beforeTraitDefinition;
		traitAfter: afterTraitDefinition;
		packageBefore: beforePackageDefinition;
		packageAfter: afterPackageDefinition;
		yourself
]

{ #category : 'instance creation' }
RwTraitMove class >> traitBefore: beforeTraitDefinition traitAfter: afterTraitDefinition packageBefore: beforePackageDefinition packageAfter: afterPackageDefinition projectBefore: beforeProjectDefinition projectAfter: afterProjectDefinition [

	^(self new)
		traitBefore: beforeTraitDefinition;
		traitAfter: afterTraitDefinition;
		packageBefore: beforePackageDefinition;
		packageAfter: afterPackageDefinition;
		projectBefore: beforeProjectDefinition;
		projectAfter: afterProjectDefinition;
		yourself
]

{ #category : 'printing' }
RwTraitMove >> printOn: aStream [
	super printOn: aStream.
	aStream
		nextPutAll: ' for ';
		nextPutAll: self classAfter name
]

{ #category : 'accessing' }
RwTraitMove >> traitAfter [
	^traitAfter
]

{ #category : 'accessing' }
RwTraitMove >> traitAfter: object [
	traitAfter := object
]

{ #category : 'accessing' }
RwTraitMove >> traitBefore [
	^traitBefore
]

{ #category : 'accessing' }
RwTraitMove >> traitBefore: object [
	traitBefore := object
]
