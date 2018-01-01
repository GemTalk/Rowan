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
	#instVars : [
		'propertiesModification'
	],
	#category : 'Rowan-GemStone'
}

{ #category : 'installing' }
RwGsMethodPropertiesPatch >> installPropertiesPatchNewClasses: createdClasses andExistingClasses: tempSymbols [
	" update method protocol and update loadedMethod with new compiled method"

	| className class methodDictionary oldCompiledMethod loadedMethod |
	className := classDefinition key asSymbol.
	class := createdClasses at: className
				ifAbsent: 
					[tempSymbols at: className
						ifAbsent: [self error: 'Cannot find class to update protocol for.']].
	behavior := isMeta ifTrue: [class class] ifFalse: [class].
	methodDictionary := behavior persistentMethodDictForEnv: 0.
	selector := methodDefinition selector.
	oldCompiledMethod := methodDictionary 
		at: selector 
		ifAbsent: [ self error: 'Internal error -- no existing CompileMethod found for patched method.' ].
	self setProtocol.
	loadedMethod := RwGsLoadedMethod existingForMethod: oldCompiledMethod
				ifAbsent: [
					self
						error: 'Internal error -- no existing LoadedMethod found for patched method.'].
	loadedMethod setPropertiesTo: methodDefinition properties
]

{ #category : 'initializers' }
RwGsMethodPropertiesPatch >> runInitializer [
	"noop"
]
