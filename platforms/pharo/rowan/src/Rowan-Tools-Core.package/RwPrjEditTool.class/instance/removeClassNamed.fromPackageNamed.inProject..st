class editing
removeClassNamed: className fromPackageNamed: packageName inProject: projectDefinition

	"Remove the named class definition from the named package in the given project. 
		Return the projectDefinition"

	| packageDefinition |
	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition removeClassNamed: className.
	^ projectDefinition