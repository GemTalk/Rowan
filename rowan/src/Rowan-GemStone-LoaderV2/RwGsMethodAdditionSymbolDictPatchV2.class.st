Class {
	#name : 'RwGsMethodAdditionSymbolDictPatchV2',
	#superclass : 'RwGsMethodPatchV2',
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'installing' }
RwGsMethodAdditionSymbolDictPatchV2 >> installMethod [

	self symbolDictionaryRegistry
		addNewCompiledMethod: compiledMethod
		for: behavior
		protocol: self propertiesProtocolName
		toPackageNamed: self packageName
		implementationClass: RwGsSymbolDictionaryRegistry_Implementation.
	selector := compiledMethod selector

]

{ #category : 'installing' }
RwGsMethodAdditionSymbolDictPatchV2 >> installMovedMethod: aClassMove newClassVersionPatch: newClassVersionPatch [
	"the receiver represents an existing method for a class with a new version that has moved to a new package"

	"the old method in the old class version must be removed from the loaded things in the new registry, since it was moved earlier, 
		then the regular installMethod should be performed --- new version requires that all methods be compiled"

	"https://github.com/dalehenrich/Rowan/issues/316"

	| oldClassVersion oldBehavior theRegistry |
	theRegistry := (self symbolDictionaryFor: aClassMove packageAfter name projectDefinition: aClassMove projectAfter)
		rowanSymbolDictionaryRegistry.

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
							ifNil: [
								theRegistry
									error:
										'Internal error -- no existing LoadedMethod found for deleted method.' ]
							ifNotNil: [ :oldLoadedMethod | theRegistry methodRegistry removeKey: oldCompiledMethod ] ] ].

	theRegistry
		addNewCompiledMethod: compiledMethod
		for: behavior
		protocol: self propertiesProtocolName
		toPackageNamed: aClassMove packageAfter name
		implementationClass: RwGsSymbolDictionaryRegistry_Implementation.
	selector := compiledMethod selector
]
