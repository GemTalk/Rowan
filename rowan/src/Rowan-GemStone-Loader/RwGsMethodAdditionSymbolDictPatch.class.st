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

	| oldClassVersion oldBehavior theRegistry |
	theRegistry := self symbolDictionaryRegistry. "if the class is moved between symbol dictionaries, we have already moved the old method to the this registry"
	oldClassVersion := newClassVersionPatch oldClassVersion.
	oldBehavior := self isMeta
		ifTrue: [ oldClassVersion class ]
		ifFalse: [ oldClassVersion ].
    (oldBehavior compiledMethodAt: self methodDefinition selector otherwise: nil)
		ifNotNil: [ :oldCompiledMethod |
			"new methods will not be in the old method dictionary"
			(theRegistry methodRegistry
				at: oldCompiledMethod
				ifAbsent: [])
					ifNil: [
						theRegistry
							error:
								'Internal error -- no existing LoadedMethod found for deleted method.' ]
					ifNotNil: [ :oldLoadedMethod | theRegistry methodRegistry removeKey: oldCompiledMethod ] ].

	self symbolDictionaryRegistry
		addNewCompiledMethod: compiledMethod
		for: behavior
		protocol: self propertiesProtocolName
		toPackageNamed: self packageName
		implementationClass: RwGsSymbolDictionaryRegistry_Implementation.
	selector := compiledMethod selector

]
