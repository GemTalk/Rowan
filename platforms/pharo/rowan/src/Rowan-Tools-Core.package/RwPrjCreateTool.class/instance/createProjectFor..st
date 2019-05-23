loaded project creation
createProjectFor: projectDefinition

	"Create a new loaded project based on the given project definition. If the project exists on disk it will be used, if the project 
		does not exist on disk it will be created."

	"If project does not exist on disk, create project on disk using tonel format"

	^ self createProjectFor: projectDefinition format: 'tonel'
