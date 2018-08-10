"
No class-specific documentation for CypGsMethodAdditionPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsMethodAdditionPatch( isMeta methodDefinition classDefinition packageDefinition)

"
Class {
	#name : 'RwGsMethodAdditionSymbolDictPatch',
	#superclass : 'RwGsMethodPatch',
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'installing' }
RwGsMethodAdditionSymbolDictPatch >> installMethod [

	self symbolDictionaryRegistry
		addNewCompiledMethod: compiledMethod
		for: behavior
		protocol: self propertiesProtocolName
		toPackageNamed: self packageName
		implementationClass: RwGsSymbolDictionaryRegistry_Implementation.
	selector := compiledMethod selector

]

{ #category : 'installing' }
RwGsMethodAdditionSymbolDictPatch >> installMovedMethod: aClassMove newClassVersionPatch: newClassVersionPatch [
	"the receiver represents an existing method for a class with a new version that has moved to a new package"

	"the old method in the old class version must be removed from the loaded things, then the regular installMethod should be performed"

	"https://github.com/dalehenrich/Rowan/issues/316"

	| oldClassVersion oldBehavior oldCompiledMethod oldRegistry oldLoadedMethod |
	oldRegistry := self symbolDictionaryRegistryFor: aClassMove packageBefore name.
	oldClassVersion := newClassVersionPatch oldClassVersion.
	oldBehavior := self isMeta
		ifTrue: [ oldClassVersion class ]
		ifFalse: [ oldClassVersion ].
    oldCompiledMethod := oldBehavior compiledMethodAt: self methodDefinition selector.
	oldLoadedMethod := oldRegistry methodRegistry
		at: oldCompiledMethod
		ifAbsent: [ 
			oldRegistry
				error:
					'Internal error -- no existing LoadedMethod found for deleted method.' ].

	oldRegistry methodRegistry removeKey: oldCompiledMethod.

	self symbolDictionaryRegistry
		addNewCompiledMethod: compiledMethod
		for: behavior
		protocol: self propertiesProtocolName
		toPackageNamed: self packageName
		implementationClass: RwGsSymbolDictionaryRegistry_Implementation.
	selector := compiledMethod selector

]
