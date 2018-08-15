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
	theRegistry := self symbolDictionaryRegistryFor: aClassMove packageBefore name. 
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
							ifNil: []
							ifNotNil: [ :oldLoadedMethod | theRegistry methodRegistry removeKey: oldCompiledMethod ] ] ].

	self symbolDictionaryRegistry
		addNewCompiledMethod: compiledMethod
		for: behavior
		protocol: self propertiesProtocolName
		toPackageNamed: aClassMove packageAfter name
		implementationClass: RwGsSymbolDictionaryRegistry_Implementation.
	selector := compiledMethod selector

]
