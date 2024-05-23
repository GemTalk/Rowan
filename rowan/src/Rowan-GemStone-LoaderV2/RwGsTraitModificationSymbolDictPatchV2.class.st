Class {
	#name : 'RwGsTraitModificationSymbolDictPatchV2',
	#superclass : 'RwGsTraitPatchV2',
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

{ #category : 'actions' }
RwGsTraitModificationSymbolDictPatchV2 >> addPatchedTraitModification: aTraitModification inPackage: aPackageDefinition inProject: aProjectDefinition toPatchSet: aRwGsPatchSet [
	"Need to decide whether the patches in aClassModification warrant a new class version or 
	 simple property changes."

	| dict existingTrait afterTraitDefinition beforeTraitDefinition afterSymDict beforeSymDict |
	packageDefinition := aPackageDefinition.
	projectDefinition := aProjectDefinition.
	afterTraitDefinition := aTraitModification after.
	beforeTraitDefinition := aTraitModification before.
	dict := Rowan image symbolDictNamed: beforeTraitDefinition gs_symbolDictionary.
	existingTrait := dict
		at: beforeTraitDefinition name asSymbol
		ifAbsent: [ self error: 'Internal error. Attempt to modify a trait whose name is not bound.' ].
	afterSymDict := self symbolDictionary name asString.
	beforeSymDict := beforeTraitDefinition gs_symbolDictionary.
	beforeSymDict ~= afterSymDict
		ifTrue: [ 
			self halt: 'not implemented yet'.
			aRwGsPatchSet
				addPatchedTraitSymbolDictionaryMove: aTraitModification
				inPackage: aPackageDefinition
				inProject: aProjectDefinition ].
false ifTrue: [
	"bypass for the moment while I get method modifications handled"
	self halt: 'modifiy the existing trait in place'
]
]
