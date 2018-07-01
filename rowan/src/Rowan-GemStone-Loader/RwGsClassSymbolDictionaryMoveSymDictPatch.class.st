Class {
	#name : 'RwGsClassSymbolDictionaryMoveSymDictPatch',
	#superclass : 'RwGsClassPatch',
	#instVars : [
		'classModification'
	],
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'instance creation' }
RwGsClassSymbolDictionaryMoveSymDictPatch class >> for: aClassModification inPackage: aPackageDefinition [

	^ self new
		classModification: aClassModification;
		packageDefinition: aPackageDefinition;
		yourself

]

{ #category : 'accessing' }
RwGsClassSymbolDictionaryMoveSymDictPatch >> classModification: aClassModification [

	classModification := aClassModification.
	classDefinition := classModification after

]

{ #category : 'installing' }
RwGsClassSymbolDictionaryMoveSymDictPatch >> installSymbolDictionaryPatchFor: aPatchSet [

	| before originalSymbolDictionary assoc newSymbolDictionary theClass registry |
	theClass := Rowan globalNamed: classDefinition name.
	before := classModification before.
	originalSymbolDictionary := Rowan globalNamed: before gs_symbolDictionary.
	assoc := originalSymbolDictionary associationAt: before key asSymbol.
	registry := originalSymbolDictionary rowanSymbolDictionaryRegistry.
	registry deleteClassNamedFromPackage: classDefinition name implementationClass: RwGsSymbolDictionaryRegistry_Implementation.
	newSymbolDictionary := Rowan globalNamed: ((projectDefinition specification platformSpec at: 'gemstone')
				symbolDictNameForPackageNamed: packageDefinition name) .
	registry := newSymbolDictionary rowanSymbolDictionaryRegistry.
	registry 
		addClassAssociation: assoc 
			forClass: theClass 
			toPackageNamed: packageDefinition name
			implementationClass: RwGsSymbolDictionaryRegistry_Implementation

]

{ #category : 'installing' }
RwGsClassSymbolDictionaryMoveSymDictPatch >> installSymbolDictionaryPatchFor: aPatchSet classMove: aClassMove [

	RwGsSymbolDictionaryRegistry_Implementation moveClassFor: aClassMove

]
