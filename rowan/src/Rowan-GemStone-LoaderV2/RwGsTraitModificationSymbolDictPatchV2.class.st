Class {
	#name : 'RwGsTraitModificationSymbolDictPatchV2',
	#superclass : 'RwGsTraitPatchV2',
	#instVars : [
		'existingTrait',
		'theModificationForMove'
	],
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'actions' }
RwGsTraitModificationSymbolDictPatchV2 class >> addPatchedTraitModification: aTraitModification inPackage: aPackageDefinition inProject: aProjectDefinition toPatchSet: aRwGsPatchSet [
	^ self new
		addPatchedTraitModification: aTraitModification
		inPackage: aPackageDefinition
		inProject: aProjectDefinition
		toPatchSet: aRwGsPatchSet
]

{ #category : 'other' }
RwGsTraitModificationSymbolDictPatchV2 >> addPatchedTraitModification: aTraitModification inPackage: aPackageDefinition inProject: aProjectDefinition toPatchSet: aRwGsPatchSet [
	"Need to decide whether the patches in aClassModification warrant a new class version or 
	 simple property changes."

	| dict beforeTraitDefinition afterSymDict beforeSymDict |
	packageDefinition := aPackageDefinition.
	projectDefinition := aProjectDefinition.
	traitDefinition := aTraitModification after.
	beforeTraitDefinition := aTraitModification before.
	dict := Rowan image symbolDictNamed: beforeTraitDefinition gs_symbolDictionary.
	existingTrait := dict
		at: beforeTraitDefinition name asSymbol
		ifAbsent: [ self error: 'Internal error. Attempt to modify a trait whose name is not bound.' ].
	afterSymDict := self symbolDictionary name asString.
	beforeSymDict := beforeTraitDefinition gs_symbolDictionary.
	beforeSymDict = afterSymDict
		ifFalse: [ theModificationForMove := aTraitModification ].
	aRwGsPatchSet addPatchedTraitChanged: self
]

{ #category : 'accessing' }
RwGsTraitModificationSymbolDictPatchV2 >> existingTrait [
	^existingTrait
]

{ #category : 'accessing' }
RwGsTraitModificationSymbolDictPatchV2 >> existingTrait: object [
	existingTrait := object
]

{ #category : 'accessing' }
RwGsTraitModificationSymbolDictPatchV2 >> modificationForMove [

	^theModificationForMove
]

{ #category : 'other' }
RwGsTraitModificationSymbolDictPatchV2 >> updateChangedTraits [
	"The trait has changed shape (iv, civ, or cv)"

	self existingTrait
		ivNames: traitDefinition instVarNames
		civNames: traitDefinition classInstVarNames
		cvNames: traitDefinition classVarNames.
	(Rowan image loadedTraitForTrait: self existingTrait ifAbsent: [  ])
		ifNil: [ self error: 'No trait found for ' , self existingTrait name ]
		ifNotNil: [ :loadedTrait | loadedTrait updatePropertiesFromTraitDefinition: traitDefinition ]
]
