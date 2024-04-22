Class {
	#name : 'RwGsTraitAdditionSymbolDictPatchV2',
	#superclass : 'RwGsTraitPatchV2',
	#instVars : [
		'newTrait',
		'symbolAssociation'
	],
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'other' }
RwGsTraitAdditionSymbolDictPatchV2 >> createTraitFor: aPatchSet inSymDict: symDictName [
	newTrait := super createTraitFor: aPatchSet inSymDict: symDictName.
	symbolAssociation := aPatchSet tempAssociationFor: newTrait name.
	^ newTrait
]

{ #category : 'actions' }
RwGsTraitAdditionSymbolDictPatchV2 >> installTraitInSystem [

	"Copy the name association to the correct 
        SymbolDictionary in the live SymbolList.
        Create a LoadedTrait for the new trait, add it to the defining LoadedPackage."

	| loadedTrait |
	loadedTrait := self symbolDictionaryRegistry
		addTraitAssociation: symbolAssociation
		forTrait: self newTrait
		toPackageNamed: self packageName
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2.
	loadedTrait updatePropertiesFromTraitDefinition: self traitDefinition
]

{ #category : 'accessing' }
RwGsTraitAdditionSymbolDictPatchV2 >> newTrait [
	^newTrait
]
