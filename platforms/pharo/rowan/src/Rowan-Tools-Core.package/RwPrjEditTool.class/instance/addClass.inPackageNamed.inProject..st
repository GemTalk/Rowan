class editing
addClass: classDefinition inPackageNamed: packageName inProject: projectDefinition

	"Add the given class definition to the named package in the given project. 
		Return the projectDefinition"

	| packageDefinition |
	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition addClassDefinition: classDefinition.
	^ projectDefinition