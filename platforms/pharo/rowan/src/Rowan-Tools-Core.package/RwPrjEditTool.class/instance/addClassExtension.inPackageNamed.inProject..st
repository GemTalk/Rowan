class extension editting
addClassExtension: classExtensionDefinition inPackageNamed: packageName inProject: projectDefinition

	"Add the given class extension definition to the named package in the given project. 
		Return the projectDefinition"

	| packageDefinition |
	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.
	^ projectDefinition