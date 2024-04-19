Class {
	#name : 'RwGsClassVariableChangeSymbolDictPatchV2',
	#superclass : 'RwGsClassPatchV2',
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'installing' }
RwGsClassVariableChangeSymbolDictPatchV2 >> installPropertiesPatchSymbolListFor: aPatchSet_symbolList [

	self installPropertiesPatchSymbolListFor: aPatchSet_symbolList registry: self symbolDictionaryRegistry
]

{ #category : 'installing' }
RwGsClassVariableChangeSymbolDictPatchV2 >> installPropertiesPatchSymbolListFor: aPatchSet registry: aSymbolDictionaryRegistry [
	" update class and update loadedClass with new properties"

	| className symDict existingClass |
	className := classDefinition key asSymbol.
	symDict := aPatchSet class
		lookupSymbolDictName: self symbolDictionaryName
		in: aPatchSet createdClasses.
	existingClass := symDict
		at: className
		ifAbsent: [ 
			aPatchSet tempSymbols
				at: className
				ifAbsent: [ 
					"cannot find class ... caller can decide whether or not that is a problem"
					self error: 'Cannot find class to update class variables for.' ] ].
	aSymbolDictionaryRegistry
		updateClassProperties: existingClass
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2
]
