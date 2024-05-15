Class {
	#name : 'RwGsClassPropertiesSymDictPatchV2',
	#superclass : 'RwGsClassPatchV2',
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'actions' }
RwGsClassPropertiesSymDictPatchV2 >> addToNewClassesByNameSymbolList: newClassesByNameSymbolList [
	"noop"
]

{ #category : 'installing' }
RwGsClassPropertiesSymDictPatchV2 >> installPropertiesPatchSymbolListFor: aPatchSet_symbolList [

	self installPropertiesPatchSymbolListFor: aPatchSet_symbolList registry: self symbolDictionaryRegistry
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
			aPatchSet tempSymbols
				at: className
				ifAbsent: [ 
					"cannot find class ... caller can decide whether or not that is a problem"
					self error: 'Cannot find class ', className printString, ' for which there are properties to update.' ] ].
	createdClass := self
		createClassFor: aPatchSet
		inSymDict: self symbolDictionaryName.	"use createClassFor:, but not expected to create new class version"
	createdClass == existingClass
		ifFalse: [ 
			self
				error:
					'internal error - class changed during class property update ... should have been a class versioning patch' ].
	aSymbolDictionaryRegistry
		updateClassProperties: existingClass
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2
]
