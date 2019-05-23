class editing
removeClass: classDefinition fromPackageNamed: packageName inProjectNamed: projectName

	"Remove the given class definition to the named package in the named project. 
		Return a projectDefinition for the named project"

	^ self
		removeClass: classDefinition
		fromPackageNamed: packageName
		inProject: (Rowan image loadedProjectNamed: projectName) asDefinition
