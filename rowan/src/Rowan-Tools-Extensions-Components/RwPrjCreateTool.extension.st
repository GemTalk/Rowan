Extension { #name : 'RwPrjCreateTool' }

{ #category : '*rowan-tools-extensions-components' }
RwPrjCreateTool >> createComponentProject: componentProjectDefinition [

	"Create create new repository on `disk`, based on the given repository definition."

	self createProjectRepository: componentProjectDefinition projectRef.

	componentProjectDefinition exportProjects.
	componentProjectDefinition exportComponents.
	componentProjectDefinition exportPackages.

	componentProjectDefinition register.

	^ componentProjectDefinition
]

{ #category : '*rowan-tools-extensions-components' }
RwPrjCreateTool >> createComponentProjectNamed: projectName [
	"Create a new component project with all of the default attributes"

	^ RwComponentProjectDefinition projectName: projectName
]
