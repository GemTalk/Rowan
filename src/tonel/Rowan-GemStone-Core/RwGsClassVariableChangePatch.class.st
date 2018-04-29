"
No class-specific documentation for CypGsClassVariableChangePatch, hierarchy is: 
Object
  CypGsPatch
    CypGsClassVariableChangePatch( oldClassDefinition newClassDefinition)

"
Class {
	#name : 'RwGsClassVariableChangePatch',
	#superclass : 'RwGsClassPatch',
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'actions' }
RwGsClassVariableChangePatch >> createClassFor: aPatchSet [

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
RwGsClassVariableChangePatch >> installPropertiesPatchFor: aPatchSet [

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
