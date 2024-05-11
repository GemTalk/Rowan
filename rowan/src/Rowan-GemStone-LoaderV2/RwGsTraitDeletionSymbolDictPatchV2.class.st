Class {
	#name : 'RwGsTraitDeletionSymbolDictPatchV2',
	#superclass : 'RwGsTraitPatchV2',
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'instance creation' }
RwGsTraitDeletionSymbolDictPatchV2 class >> for: aTraitDefinition [

	^self new traitDefinition: aTraitDefinition
]

{ #category : 'actions' }
RwGsTraitDeletionSymbolDictPatchV2 >> addToNewClassesByNameSymbolList: newClassesByNameSymbolList [
	"noop"
]

{ #category : 'deleting' }
RwGsTraitDeletionSymbolDictPatchV2 >> deleteTraitFromSystem [

	self symbolDictionaryRegistry deleteTraitNamedFromPackage: self traitName implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2
]

{ #category : 'accessing' }
RwGsTraitDeletionSymbolDictPatchV2 >> symbolDictionary [
	^ Rowan image symbolDictNamed: self symbolDictionaryName
]

{ #category : 'accessing' }
RwGsTraitDeletionSymbolDictPatchV2 >> symbolDictionaryFor: aPackageName [
	self error: 'no longer implemented'
]

{ #category : 'accessing' }
RwGsTraitDeletionSymbolDictPatchV2 >> symbolDictionaryName [
	"because this is a deletion, we can look up the symbol dictionariy directly"

	| traitName trait | 
	traitName := self traitName.
	trait := Rowan globalNamed: traitName.
	Rowan image 
		loadedTraitAndSymbolDictionaryForTrait: trait 
		ifPresent: [:symDict :loadedClass | ^symDict  name]
		ifAbsent: [ self error: 'No symbol dictionary found for the trait ', traitName printString ].
]
