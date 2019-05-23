loaded project creation
createProjectFor: projectDefinition format: format root: rootPath

	"Create a new loaded project based on the given project definition. If the project exists on disk at <rootPath> it will be used, if the project 
		does not exist on disk it will be created."

	| projectName utils |
	projectName := projectDefinition name.
	utils := self fileUtilities.
	projectDefinition repositoryRootPath:  rootPath , utils pathNameDelimiter , projectName.
	^ self createProjectFor: projectDefinition format: format
