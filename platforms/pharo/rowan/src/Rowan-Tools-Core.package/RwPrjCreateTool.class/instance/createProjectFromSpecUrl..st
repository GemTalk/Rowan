loaded project creation
createProjectFromSpecUrl: fileUrlOrString

	"Create a new loaded project based on the given <fileUrlOrString>. If the project exists on disk it will be used, if the project 
		does not exist on disk it will be created."

	| projectDefinition |
	projectDefinition := self createProjectDefinitionFromSpecUrl: fileUrlOrString.
	^ self createProjectFor: projectDefinition
