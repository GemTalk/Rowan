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
RwGsClassSymbolDictionaryMoveSymDictPatch >> installSymbolDictionaryPatchFor: aPatchSet classMove: classMove [

	| before originalSymbolDictionary assoc newSymbolDictionary theClass oldRegistry newRegistry 
		loadedClass theBehavior |
	theClass := Rowan globalNamed: classDefinition name.
	before := classModification before.
	originalSymbolDictionary := Rowan globalNamed: before gs_symbolDictionary.
	assoc := originalSymbolDictionary associationAt: before key asSymbol.
	oldRegistry := originalSymbolDictionary rowanSymbolDictionaryRegistry.
	oldRegistry 
		deleteClassNamedFromPackage: classDefinition name 
		implementationClass: RwGsSymbolDictionaryRegistry_Implementation.
	newSymbolDictionary := Rowan globalNamed: ((projectDefinition specification platformSpec at: 'gemstone')
				symbolDictNameForPackageNamed: packageDefinition name) .
	newRegistry := newSymbolDictionary rowanSymbolDictionaryRegistry.
	newRegistry 
		addClassAssociation: assoc 
			forClass: theClass 
			toPackageNamed: packageDefinition name
			implementationClass: RwGsSymbolDictionaryRegistry_Implementation.
"need to move the instance and class methods for the loaded Class ... extension methods?"
	loadedClass := Rowan image loadedClassNamed: classMove classBefore name.
self halt.
	loadedClass loadedInstanceMethods values do: [:loadedMethod |
		| compiledMethod |
		compiledMethod := theClass compiledMethodAt: loadedMethod selector asSymbol.
		oldRegistry methodRegistry removeKey: compiledMethod.
		newRegistry methodRegistry at: compiledMethod put: loadedMethod ].
	theBehavior := theClass class.
	loadedClass loadedClassMethods values do: [:loadedMethod |
		| compiledMethod |
		compiledMethod := theBehavior compiledMethodAt: loadedMethod selector asSymbol.
		oldRegistry methodRegistry removeKey: compiledMethod.
		newRegistry methodRegistry at: compiledMethod put: loadedMethod ].

]
