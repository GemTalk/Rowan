Class {
	#name : 'RwGsClassPropertiesSymDictPatchV2',
	#superclass : 'RwGsClassPatchV2',
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'actions' }
RwGsClassPropertiesSymDictPatchV2 >> addToNewClassesByName: aDictionary [

	"noop"

	
]

{ #category : 'actions' }
RwGsClassPropertiesSymDictPatchV2 >> addToNewClassesByNameSymbolList: newClassesByNameSymbolList [
	"noop"
]

{ #category : 'installing' }
RwGsClassPropertiesSymDictPatchV2 >> installPropertiesPatchFor: aPatchSet [

	self installPropertiesPatchFor: aPatchSet registry: self symbolDictionaryRegistry

]

{ #category : 'patching moved classes' }
RwGsClassPropertiesSymDictPatchV2 >> installPropertiesPatchFor: aPatchSet classMove: aClassMove [

	| theRegistry |
	theRegistry := (self symbolDictionaryFor: aClassMove packageAfter name projectDefinition: aClassMove projectAfter)
		rowanSymbolDictionaryRegistry.
	self installPropertiesPatchFor: aPatchSet registry: theRegistry
]

{ #category : 'installing' }
RwGsClassPropertiesSymDictPatchV2 >> installPropertiesPatchFor: aPatchSet registry: aSymbolDictionaryRegistry [

	" update class and update loadedClass with new properties"

	| className existingClass createdClass |
	className := classDefinition key asSymbol.
	existingClass := aPatchSet createdClasses
		at: className
		ifAbsent: [ 
			aPatchSet tempSymbols
				at: className
				ifAbsent: [ self error: 'Cannot find class to update properties for.' ] ].
	createdClass := self createClassFor: aPatchSet.	"use createClassFor:, but not expected to create new class version"
	createdClass == existingClass
		ifFalse: [ 
			self
				error:
					'internal error - class changed during class property update ... should have been a class versioning patch' ].
	aSymbolDictionaryRegistry updateClassProperties: existingClass implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2
]

{ #category : 'installing' }
RwGsClassPropertiesSymDictPatchV2 >> installPropertiesPatchSymbolListFor: aPatchSet_symbolList [

	self installPropertiesPatchSymbolListFor: aPatchSet_symbolList registry: self symbolDictionaryRegistry
]

{ #category : 'patching moved classes' }
RwGsClassPropertiesSymDictPatchV2 >> installPropertiesPatchSymbolListFor: aPatchSet classMove: aClassMove [

	| theRegistry |
	theRegistry := (self symbolDictionaryFor: aClassMove packageAfter name projectDefinition: aClassMove projectAfter)
		rowanSymbolDictionaryRegistry.
	self installPropertiesPatchSymbolListFor: aPatchSet registry: theRegistry
]

{ #category : 'installing' }
RwGsClassPropertiesSymDictPatchV2 >> installPropertiesPatchSymbolListFor: aPatchSet registry: aSymbolDictionaryRegistry [
	" update class and update loadedClass with new properties"

	| className existingClass createdClass symDict |
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
							self error: 'Cannot find class to update properties for.' ]
						ifNotNil: [ :assoc | assoc value ] ] ].
	createdClass := self createClassFor: aPatchSet.	"use createClassFor:, but not expected to create new class version"
	createdClass == existingClass
		ifFalse: [ 
			self
				error:
					'internal error - class changed during class property update ... should have been a class versioning patch' ].
	aSymbolDictionaryRegistry
		updateClassProperties: existingClass
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2
]
