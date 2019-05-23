class editing
addClass: classDefinition inPackageNamed: packageName inProjectNamed: projectName

	"Add the given class definition to the named package in the named project. 
		Return a projectDefinition for the named project"

	^ self
		addClass: classDefinition
		inPackageNamed: packageName
		inProject: (Rowan image loadedProjectNamed: projectName) asDefinition
