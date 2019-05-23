delete projects by name
deleteProjectNamed: projectName

	"Remove the classes and methods managed by the project from the loaded things."

	"The classes and methods are removed from the system"

	"If there are subclasses of classes in other projects the delete will fail ... deleteProjectDefinitionSet: to delete multiple projects"

	| loadedProject |

	loadedProject := Rowan image loadedProjectNamed: projectName.

	^ self deleteProjectDefinition: loadedProject asDefinition
