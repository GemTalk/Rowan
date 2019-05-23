delete projects by name
forceDeleteProjectNamed: projectName

	"Remove the classes and methods managed by the project from the loaded things."

	"The classes and methods are removed from the system"

	"If there are subclasses of classes in other projects not in the set, the delete will NOT fail."

	| loadedProject |

	loadedProject := Rowan image loadedProjectNamed: projectName.

	^ self forceDeleteProjectDefinition: loadedProject asDefinition
