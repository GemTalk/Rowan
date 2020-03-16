Class {
	#name : 'RwGsClassSymbolDictionaryMoveSymDictPatchV2',
	#superclass : 'RwGsClassPatchV2',
	#instVars : [
		'classModification'
	],
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'instance creation' }
RwGsClassSymbolDictionaryMoveSymDictPatchV2 class >> for: aClassModification inPackage: aPackageDefinition [

	^ self new
		classModification: aClassModification;
		packageDefinition: aPackageDefinition;
		yourself

]

{ #category : 'accessing' }
RwGsClassSymbolDictionaryMoveSymDictPatchV2 >> classModification [

	^classModification

]

{ #category : 'accessing' }
RwGsClassSymbolDictionaryMoveSymDictPatchV2 >> classModification: aClassModification [

	classModification := aClassModification.
	classDefinition := classModification after

]

{ #category : 'installing' }
RwGsClassSymbolDictionaryMoveSymDictPatchV2 >> installSymbolDictionaryPatchFor: aPatchSet [

	| before originalSymbolDictionary assoc newSymbolDictionary theClass registry loadedClass |
	theClass := Rowan globalNamed: classDefinition name.
	before := classModification before.
	originalSymbolDictionary := Rowan globalNamed: before gs_symbolDictionary.
	assoc := originalSymbolDictionary associationAt: before key asSymbol.
	registry := originalSymbolDictionary rowanSymbolDictionaryRegistry.
	registry deleteClassNamedFromPackage: classDefinition name implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2.
	newSymbolDictionary := Rowan image symbolDictNamed: (projectDefinition symbolDictNameForPackageNamed: packageDefinition name) .
	registry := newSymbolDictionary rowanSymbolDictionaryRegistry.
	loadedClass := registry 
		addClassAssociation: assoc 
			forClass: theClass 
			toPackageNamed: packageDefinition name
			implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2.
	loadedClass updatePropertiesFromClassDefinition: self classDefinition
]

{ #category : 'installing' }
RwGsClassSymbolDictionaryMoveSymDictPatchV2 >> installSymbolDictionaryPatchFor: aPatchSet classMove: aClassMove [

	RwGsSymbolDictionaryRegistry_ImplementationV2 moveClassFor: aClassMove
]
