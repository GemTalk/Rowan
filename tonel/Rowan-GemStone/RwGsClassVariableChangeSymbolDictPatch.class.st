"
No class-specific documentation for CypGsClassVariableChangePatch, hierarchy is: 
Object
  CypGsPatch
    CypGsClassVariableChangePatch( oldClassDefinition newClassDefinition)

"
Class {
	#name : 'RwGsClassVariableChangeSymbolDictPatch',
	#superclass : 'RwGsClassPatch',
	#category : 'Rowan-GemStone'
}

{ #category : 'actions' }
RwGsClassVariableChangeSymbolDictPatch >> createClassFor: aPatchSet [

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
RwGsClassVariableChangeSymbolDictPatch >> installPropertiesPatchFor: aPatchSet [

	" update class and update loadedClass with new properties"

	| className existingClass |
	className := classDefinition key asSymbol.
	existingClass := aPatchSet createdClasses
		at: className
		ifAbsent: [ 
			aPatchSet tempSymbols
				at: className
				ifAbsent: [ self error: 'Cannot find class to update properties for.' ] ].
	packageSymbolDictionary updateClassProperties: existingClass
]
