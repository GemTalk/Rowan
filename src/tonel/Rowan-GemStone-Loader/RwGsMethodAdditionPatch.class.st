"
No class-specific documentation for CypGsMethodAdditionPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsMethodAdditionPatch( isMeta methodDefinition classDefinition packageDefinition)

"
Class {
	#name : 'RwGsMethodAdditionPatch',
	#superclass : 'RwGsMethodPatch',
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'private-installing' }
RwGsMethodAdditionPatch >> createLoadedMethod [

	| existing new |
	existing := RwGsLoadedMethod existingForMethod: compiledMethod
				ifAbsent: [nil].
	existing
		ifNotNil: 
			[self
				error: 'Internal error -- existing LoadedMethod found for new method.'].
	new := RwGsLoadedMethod newNamed: selector.
	new
		handle: compiledMethod;
		classIsMeta: isMeta;
		registerYourself.
	^new
]

{ #category : 'private-installing' }
RwGsMethodAdditionPatch >> installLoadedMethod [

	| loadedMethod loadedClassOrExtension |
	loadedMethod := self createLoadedMethod.
	loadedClassOrExtension := self loadedClassOrExtensionForMethod.
	loadedClassOrExtension addLoadedMethod: loadedMethod.
	loadedMethod setPropertiesTo: methodDefinition properties
]

{ #category : 'installing' }
RwGsMethodAdditionPatch >> installMethod [

	self
		updateMethodDictionary;
		setProtocol;
		installLoadedMethod
]

{ #category : 'private-installing' }
RwGsMethodAdditionPatch >> updateMethodDictionary [

	| methodDictionary |
	methodDictionary := behavior persistentMethodDictForEnv: 0.
	selector := compiledMethod selector.
	methodDictionary at: selector put: compiledMethod
]
