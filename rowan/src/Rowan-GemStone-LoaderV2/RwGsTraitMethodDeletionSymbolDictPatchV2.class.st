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
| loadedTrait |
	self
		primeBehaviorNewTraitsSymbolList: createdTraits
		andExistingTraits: tempSymbols.
	self traitInstance
		ifNil: [ 
			self
				error:
					'Trait ' , self traitName printString , ' not found in the symbol dictionary '
						, self symbolDictionaryName printString , ' associated with the method '
						, methodDefinition selector printString ].
	loadedTrait := self symbolDictionaryRegistry loadedTraitForTrait: self traitInstance.

	isMeta
		ifTrue: [ 
			self traitInstance classTrait removeSelector: methodDefinition selector.
self halt: 'Now delete the loaded method for that selector from the loadedTrait'.
"
			loadedMethod := RwGsLoadedSymbolDictTraitMethod
				loadedClassMethodForTrait: self traitInstance classTrait
				source: sourceString
				protocol: protocol.
"
 ]
		ifFalse: [ 
			self traitInstance removeSelector: methodDefinition selector.
self halt: 'Now delete the loaded method for that selector from the loadedTrait'.
"
		loadedMethod := RwGsLoadedSymbolDictTraitMethod
				loadedMethodForTrait: self traitInstance
				source: sourceString
				protocol: protocol.
"
 ].
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
