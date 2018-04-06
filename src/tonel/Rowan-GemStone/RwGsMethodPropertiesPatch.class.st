"
No class-specific documentation for CypGsMethodPropertiesPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsMethodPatch( isMeta methodDefinition classDefinition packageDefinition)
      CypGsMethodPropertiesPatch( propertiesModification)

"
Class {
	#name : 'RwGsMethodPropertiesPatch',
	#superclass : 'RwGsMethodPatch',
	#category : 'Rowan-GemStone'
}

{ #category : 'installing' }
RwGsMethodPropertiesPatch >> installPropertiesPatchNewClasses: createdClasses andExistingClasses: tempSymbols [

	" update method protocol and update loadedMethod with new compiled method"

	| methodDictionary oldCompiledMethod loadedMethod |
	self primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols.
	behavior
		ifNil: [ self error: 'Class ' , self className printString , ' not found.' ].

	methodDictionary := behavior persistentMethodDictForEnv: 0.
	selector := methodDefinition selector.
	oldCompiledMethod := methodDictionary
		at: selector
		ifAbsent: [ 
			self
				error:
					'Internal error -- no existing CompileMethod found for patched method.' ].
	self setProtocol.
	loadedMethod := RwGsLoadedMethod
		existingForMethod: oldCompiledMethod
		ifAbsent: [ 
			self
				error:
					'Internal error -- no existing LoadedMethod found for patched method.' ].
	loadedMethod setPropertiesTo: methodDefinition properties
]

{ #category : 'initializers' }
RwGsMethodPropertiesPatch >> runInitializer [
	"noop"
]
