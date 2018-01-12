"
No class-specific documentation for CypGsClassPropertiesPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsClassPatch( classDefinition packageDefinition)
      CypGsClassPropertiesPatch( propertiesModification)

"
Class {
	#name : 'RwGsClassPropertiesPatch',
	#superclass : 'RwGsClassPatch',
	#category : 'Rowan-GemStone'
}

{ #category : 'actions' }
RwGsClassPropertiesPatch >> addToNewClassesByName: aDictionary [

	"noop"

	
]

{ #category : 'installing' }
RwGsClassPropertiesPatch >> installPropertiesPatchFor: aPatchSet [
	" update class and update loadedClass with new properties"

	| className existingClass createdClass loadedClass |
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
	loadedClass := RwGsLoadedClass existingForClass: existingClass.
	loadedClass
		updatePropertiesFromClass;
		updateForCategoryChange
]
