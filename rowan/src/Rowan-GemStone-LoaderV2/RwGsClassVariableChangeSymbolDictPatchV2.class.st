Class {
	#name : 'RwGsClassVariableChangeSymbolDictPatchV2',
	#superclass : 'RwGsClassPatchV2',
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'actions' }
RwGsClassVariableChangeSymbolDictPatchV2 >> createClassFor: aPatchSet [

	"we're using createClassFor: to apply the class variable modifications. not expected to 
		create a new class version"

	| oldClassVersion newClassVersion |
	oldClassVersion := self oldClassVersion.
	newClassVersion := self privateCreateClassFor: aPatchSet.
	oldClassVersion == newClassVersion
		ifFalse: [ 
			self
				error:
					'internal error - unexpected new class version for class variable modification' ].
	^ oldClassVersion

]

{ #category : 'installing' }
RwGsClassVariableChangeSymbolDictPatchV2 >> installPropertiesPatchFor: aPatchSet [

	self installPropertiesPatchFor: aPatchSet registry: self symbolDictionaryRegistry

]

{ #category : 'installing' }
RwGsClassVariableChangeSymbolDictPatchV2 >> installPropertiesPatchFor: aPatchSet registry: aSymbolDictionaryRegistry [

	" update class and update loadedClass with new properties"

	| className existingClass |
	className := classDefinition key asSymbol.
	existingClass := aPatchSet createdClasses
		at: className
		ifAbsent: [ 
			aPatchSet tempSymbols
				at: className
				ifAbsent: [ self error: 'Cannot find class to update properties for.' ] ].
	aSymbolDictionaryRegistry updateClassProperties: existingClass  implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2
]

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
			(aPatchSet class
				lookupSymbolDictName: self symbolDictionaryName
				in: aPatchSet tempSymbolList)
				at: className
				ifAbsent: [ 
					(aPatchSet tempSymbolList resolveSymbol: className)
						ifNil: [ 
							"cannot find class ... caller can decide whether or not that is a problem"
							self error: 'Cannot find class to update class variables for.' ]
						ifNotNil: [ :assoc | assoc value ] ] ].
	aSymbolDictionaryRegistry
		updateClassProperties: existingClass
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2
]
