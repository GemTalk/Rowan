"
No class-specific documentation for CypGsMethodAdditionPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsMethodAdditionPatch( isMeta methodDefinition classDefinition packageDefinition)

"
Class {
	#name : 'RwGsMethodExtensionSymbolDictPatch',
	#superclass : 'RwGsMethodPatch',
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'installing' }
RwGsMethodExtensionSymbolDictPatch >> installMethod [

	self symbolDictionaryRegistry
		addExtensionCompiledMethod: compiledMethod
		for: behavior
		protocol: self propertiesProtocolName
		toPackageNamed: self packageName
		 implementationClass: RwGsSymbolDictionaryRegistry_Implementation.
	selector := compiledMethod selector

]

{ #category : 'installing' }
RwGsMethodExtensionSymbolDictPatch >> installMovedMethod: aMethodMove newClassVersionPatch: newClassVersionPatch [
	"the receiver represents an existing method for a class with a new version that has moved to a new package"

	"the old method in the old class version must be removed from the loaded things, then the regular installMethod should be performed"

	"https://github.com/dalehenrich/Rowan/issues/316"

	| oldClassVersion oldBehavior theRegistry |
	theRegistry := self symbolDictionaryRegistry. "if the class is moved between symbol dictionaries, we have already moved the old method to the this registry"
	oldClassVersion := newClassVersionPatch oldClassVersion.
	oldClassVersion ~~ newClassVersionPatch newClassVersion
		ifTrue: [ 
			"only do this work if we truly have a new class version"
			oldBehavior := self isMeta
				ifTrue: [ oldClassVersion class ]
				ifFalse: [ oldClassVersion ].
			(oldBehavior compiledMethodAt: self methodDefinition selector otherwise: nil)
				ifNotNil: [ :oldCompiledMethod |
					"new methods will not be in the old method dictionary"
					(theRegistry methodRegistry
						at: oldCompiledMethod
						ifAbsent: [])
							ifNil: [ "if old method is not in the method registry, then that is what we want anyway" ]
							ifNotNil: [ :oldLoadedMethod | theRegistry methodRegistry removeKey: oldCompiledMethod ] ] ].

	self symbolDictionaryRegistry
		addExtensionCompiledMethod: compiledMethod
		for: behavior
		protocol: self propertiesProtocolName
		toPackageNamed: aMethodMove packageAfter name
		implementationClass: RwGsSymbolDictionaryRegistry_Implementation.
	selector := compiledMethod selector

]
