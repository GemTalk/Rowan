class extension editting
addClassExtension: classExtensionDefinition inPackageNamed: packageName inProjectNamed: projectName

	"Add the given class extension definition to the named package in the named project. 
		Return a projectDefinition for the named project"

	^ self
		addClassExtension: classExtensionDefinition
		inPackageNamed: packageName
		inProject: (Rowan image loadedProjectNamed: projectName) asDefinition
