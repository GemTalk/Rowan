Class {
	#name : 'RwTraitMethodMove',
	#superclass : 'RwMove',
	#instVars : [
		'traitBefore',
		'traitAfter',
		'methodBefore',
		'methodAfter',
		'isMeta'
	],
	#category : 'Rowan-Core'
}

{ #category : 'unknown' }
RwTraitMethodMove class >> methodBefore: beforeMethodDefinition methodAfter: afterMethodDefinition traitBefore: beforeTraitDefinition traitAfter: afterTraitDefinition packageBefore: beforePackageDefinition packageAfter: afterPackageDefinition isMeta: isMeta [
	^ self new
		methodBefore: beforeMethodDefinition;
		methodAfter: afterMethodDefinition;
		packageBefore: beforePackageDefinition;
		packageAfter: afterPackageDefinition;
		traitBefore: beforeTraitDefinition;
		traitAfter: afterTraitDefinition;
		isMeta: isMeta;
		yourself
]

{ #category : 'unknown' }
RwTraitMethodMove class >> methodBefore: beforeMethodDefinition methodAfter: afterMethodDefinition traitBefore: beforeTraitDefinition traitAfter: afterTraitDefinition packageBefore: beforePackageDefinition packageAfter: afterPackageDefinition projectBefore: beforeProjectDefinition projectAfter: afterProjectDefinition isMeta: isMeta [
	^ self new
		methodBefore: beforeMethodDefinition;
		methodAfter: afterMethodDefinition;
		traitBefore: beforeTraitDefinition;
		traitAfter: afterTraitDefinition;
		packageBefore: beforePackageDefinition;
		packageAfter: afterPackageDefinition;
		projectBefore: beforeProjectDefinition;
		projectAfter: afterProjectDefinition;
		isMeta: isMeta;
		yourself
]

{ #category : 'Accessing' }
RwTraitMethodMove >> classOrExtensionAfter [
	^ self traitAfter
]

{ #category : 'Accessing' }
RwTraitMethodMove >> classOrExtensionBefore [
	^ self traitBefore
]

{ #category : 'Accessing' }
RwTraitMethodMove >> isMeta [
	^isMeta

]

{ #category : 'Updating' }
RwTraitMethodMove >> isMeta: newValue [
	isMeta := newValue

]

{ #category : 'Accessing' }
RwTraitMethodMove >> methodAfter [
	^methodAfter

]

{ #category : 'Updating' }
RwTraitMethodMove >> methodAfter: newValue [
	methodAfter := newValue

]

{ #category : 'Accessing' }
RwTraitMethodMove >> methodBefore [
	^methodBefore

]

{ #category : 'Updating' }
RwTraitMethodMove >> methodBefore: newValue [
	methodBefore := newValue

]

{ #category : 'accessing' }
RwTraitMethodMove >> traitAfter [
	^traitAfter
]

{ #category : 'accessing' }
RwTraitMethodMove >> traitAfter: object [
	traitAfter := object
]

{ #category : 'accessing' }
RwTraitMethodMove >> traitBefore [
	^traitBefore
]

{ #category : 'accessing' }
RwTraitMethodMove >> traitBefore: object [
	traitBefore := object
]
