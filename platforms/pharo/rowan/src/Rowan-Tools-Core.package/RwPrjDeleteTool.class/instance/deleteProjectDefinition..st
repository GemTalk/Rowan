delete project definitions
deleteProjectDefinition: projectDefinition

	"Remove the classes and methods managed by the project definition."

	"The classes and methods are removed from the system"

	"If there are subclasses of classes in other projects the delete will fail ... deleteProjectDefinitionSet: to delete multiple projects"

	| projectSetDefinition |
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.

	^ self deleteProjectSetDefinition: projectSetDefinition
