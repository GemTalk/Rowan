Extension { #name : 'RwAbstractClassDefinition' }

{ #category : '*rowan-gemstone-definitions-extensions' }
RwAbstractClassDefinition >> modificationForcingNewVersion [

	^ self _modificationForcingNewClassVersion before: self after: self
]
