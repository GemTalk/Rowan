Extension { #name : 'RwPlatform' }

{ #category : '*rowan-corev4' }
RwPlatform >> newV4ProjectNamed: projectName [
	"Create a new project that uses project spec and component classes that are compatible with Rowan v2"

	^ RwDefinedProject newV4Named: projectName
]
