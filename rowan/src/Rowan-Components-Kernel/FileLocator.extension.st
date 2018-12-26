Extension { #name : 'FileLocator' }

{ #category : '*rowan-components-kernel' }
FileLocator class >> rowanProjectsHome [
	"Answer the path to $ROWAN_PROJECTS_HOME"

	^ self origin: #rowanProjectsHome
]
