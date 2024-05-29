Class {
	#name : 'RwGsTraitMethodDeletionSymbolDictPatchV2',
	#superclass : 'RwGsMethodDeletionSymbolDictPatchV2',
	#instVars : [
		'traitDefinition',
		'traitInstance'
	],
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'deleting' }
RwGsTraitMethodDeletionSymbolDictPatchV2 >> deleteNewTraitsSymbolList: createdTraits andExistingTraits: tempSymbols [
	"remove the method from deleted things"

	| loadedTrait loadedMethod |
	self
		primeBehaviorNewTraitsSymbolList: createdTraits
		andExistingTraits: tempSymbols.
	self traitInstance
		ifNil: [ 
			"trait has been deleted from system ... no more clean up needed"
			^ self ].
	loadedTrait := self symbolDictionaryRegistry
		loadedTraitForTrait: self traitInstance.
	loadedMethod := loadedTrait
		loadedMethodAt: methodDefinition selector
		isMeta: isMeta.

	isMeta
		ifTrue: [ 
			self traitInstance classTrait removeSelector: methodDefinition selector.
			loadedTrait removeLoadedClassMethod: loadedMethod ]
		ifFalse: [ 
			self traitInstance removeSelector: methodDefinition selector.
			loadedTrait removeLoadedInstanceMethod: loadedMethod ]
]

{ #category : 'accessing' }
RwGsTraitMethodDeletionSymbolDictPatchV2 >> traitDefinition [
	^traitDefinition
]

{ #category : 'accessing' }
RwGsTraitMethodDeletionSymbolDictPatchV2 >> traitDefinition: object [
	traitDefinition := object
]

{ #category : 'accessing' }
RwGsTraitMethodDeletionSymbolDictPatchV2 >> traitInstance [
	^traitInstance
]

{ #category : 'accessing' }
RwGsTraitMethodDeletionSymbolDictPatchV2 >> traitInstance: object [
	traitInstance := object
]

{ #category : 'accessing' }
RwGsTraitMethodDeletionSymbolDictPatchV2 >> traitName [

	^ self traitDefinition key
]
