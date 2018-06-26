Extension { #name : 'RwAbstractClassDefinition' }

{ #category : '*rowan-gemstone-definitions' }
RwAbstractClassDefinition >> modificationForcingNewVersion [

	^ self _modificationForcingNewClassVersion before: self after: self
]
