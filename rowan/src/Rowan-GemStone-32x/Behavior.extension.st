Extension { #name : 'Behavior' }

{ #category : '*rowan-gemstone-35x' }
Behavior >> rwGuaranteePersistentMethodDictForEnv: envId [
	"in 3.2, the method persistentMethodDictForEnv: always returns a GsMethodDictionary,
		as classes are created with a GsMethodDictionary for envId 0"

	^ self persistentMethodDictForEnv: envId
]
