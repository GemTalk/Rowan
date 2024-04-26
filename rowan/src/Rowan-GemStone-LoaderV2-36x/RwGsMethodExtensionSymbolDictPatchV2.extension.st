Extension { #name : 'RwGsMethodExtensionSymbolDictPatchV2' }

{ #category : '*rowan-gemstone-loaderv2-36x' }
RwGsMethodExtensionSymbolDictPatchV2 >> installMovedMethod: aMethodMove newClassVersionPatch: newClassVersionPatch [
	"the receiver represents an existing method for a class with a new version that has moved to a new package"

	"the old method in the old class version must be removed from the loaded things in the new registry, since it was moved earlier, 
		then the regular installMethod should be performed --- new version requires that all methods be compiled"

	"https://github.com/dalehenrich/Rowan/issues/316"

	| oldClassVersion oldBehavior theRegistry newBehavior |
	theRegistry := (self
		symbolDictionaryFor: aMethodMove packageAfter name
		projectDefinition: aMethodMove projectAfter) rowanSymbolDictionaryRegistry.
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
					oldCompiledMethod _owner: nil ] ].

	newBehavior := behavior isMeta
		ifTrue: [ newClassVersionPatch newClassVersion class ]
		ifFalse: [ newClassVersionPatch newClassVersion ].
	theRegistry
		addExtensionCompiledMethod: compiledMethod
		for: newBehavior
		protocol: self propertiesProtocolName
		toPackageNamed: aMethodMove packageAfter name
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2.
	selector := compiledMethod selector
]
