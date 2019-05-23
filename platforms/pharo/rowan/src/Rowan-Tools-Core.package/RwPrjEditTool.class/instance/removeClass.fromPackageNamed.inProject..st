class editing
removeClass: classDefinition fromPackageNamed: packageName inProject: projectDefinition

	"Remove the given class definition from the named package in the given project. 
		Return the projectDefinition"

	| packageDefinition |
	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition removeClassDefinition: classDefinition.
	^ projectDefinition