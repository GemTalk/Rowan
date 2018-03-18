Extension { #name : 'RwAbstractClassDefinition' }

{ #category : '*rowan-gemstone' }
RwAbstractClassDefinition >> modificationForcingNewVersion [

	^ self _modificationForcingNewClassVersion before: self after: self
]
