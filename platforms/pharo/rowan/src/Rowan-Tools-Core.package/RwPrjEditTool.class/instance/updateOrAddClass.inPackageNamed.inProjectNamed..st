class editing
updateOrAddClass: classDefinition inPackageNamed: packageName inProjectNamed: projectName

	"Update or add the given class definition to the named package in the named project. 
		Return a projectDefinition for the named project"

	^ self
		updateOrAddClass: classDefinition
		inPackageNamed: packageName
		inProject: (Rowan image loadedProjectNamed: projectName) asDefinition
