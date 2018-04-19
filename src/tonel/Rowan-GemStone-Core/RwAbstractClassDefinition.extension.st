Extension { #name : 'RwAbstractClassDefinition' }

{ #category : '*rowan-gemstone-core' }
RwAbstractClassDefinition >> modificationForcingNewVersion [

	^ self _modificationForcingNewClassVersion before: self after: self
]
