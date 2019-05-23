component project defintion creation
createComponentProject: componentProjectDefinition

	"Create create new repository on `disk`, based on the given repository definition."

	self createProjectRepository: componentProjectDefinition projectRef.

	componentProjectDefinition exportProjects.
	componentProjectDefinition exportComponents.
	componentProjectDefinition exportPackages.

	componentProjectDefinition register.

	^ componentProjectDefinition