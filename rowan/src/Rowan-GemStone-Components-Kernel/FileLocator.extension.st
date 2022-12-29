Extension { #name : 'FileLocator' }

{ #category : '*rowan-gemstone-components-kernel' }
FileLocator class >> rowanProjectsHome [
	"Answer the path to $ROWAN_PROJECTS_HOME"

	^ self origin: #rowanProjectsHome
]
