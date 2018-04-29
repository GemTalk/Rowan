"
No class-specific documentation for CypGsClassPropertiesPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsClassPatch( classDefinition packageDefinition)
      CypGsClassPropertiesPatch( propertiesModification)

"
Class {
	#name : 'RwGsClassPropertiesSymDictPatch',
	#superclass : 'RwGsClassPatch',
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'actions' }
RwGsClassPropertiesSymDictPatch >> addToNewClassesByName: aDictionary [

	"noop"

	
]

{ #category : 'installing' }
RwGsClassPropertiesSymDictPatch >> installPropertiesPatchFor: aPatchSet [

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
	self symbolDictionaryRegistry updateClassProperties: existingClass
]
