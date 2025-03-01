Extension { #name : 'Rowan' }

{ #category : '*rowan-corev4' }
Rowan class >> newV4ProjectNamed: projectName [
	"Create a new project that uses project spec and component classes that are compatible with Rowan v4"

	^ self platform newV4ProjectNamed: projectName
]
