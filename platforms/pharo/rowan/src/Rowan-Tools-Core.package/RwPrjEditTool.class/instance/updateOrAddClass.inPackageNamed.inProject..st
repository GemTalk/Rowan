class editing
updateOrAddClass: classDefinition inPackageNamed: packageName inProject: projectDefinition

	"Update or add the given class definition to the named package in the given project. 
		Return the projectDefinition"

	| packageDefinition |
	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition updateClassDefinition: classDefinition.
	^ projectDefinition