Class {
	#name : 'RwGsClassPropertiesSymDictPatchV2',
	#superclass : 'RwGsClassPatchV2',
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'actions' }
RwGsClassPropertiesSymDictPatchV2 >> addToNewClassesByName: aDictionary [

	"noop"

	
]

{ #category : 'installing' }
RwGsClassPropertiesSymDictPatchV2 >> installPropertiesPatchFor: aPatchSet [

	self installPropertiesPatchFor: aPatchSet registry: self symbolDictionaryRegistry

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
