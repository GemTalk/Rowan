class editing
removeClassNamed: className fromPackageNamed: packageName inProjectNamed: projectName

	"Remove the named class definition to the named package in the named project. 
		Return a projectDefinition for the named project"

	^ self
		removeClassNamed: className
		fromPackageNamed: packageName
		inProject: (Rowan image loadedProjectNamed: projectName) asDefinition
