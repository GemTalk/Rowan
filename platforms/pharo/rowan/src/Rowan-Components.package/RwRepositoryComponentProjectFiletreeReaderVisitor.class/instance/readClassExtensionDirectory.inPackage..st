class reading
readClassExtensionDirectory: classExtensionDirectory inPackage: packageName

	| classExtensionProperties className package |
	classExtensionProperties :=  self _readObjectFrom: classExtensionDirectory / 'properties' , 'json'.
	className := classExtensionProperties at: 'name'.
	package := currentProjectDefinition packageNamed: packageName.
	currentClassDefinition := nil.
	currentClassExtension := package 
		classExtensionDefinitionNamed: className 
		ifAbsent: [ package addClassExtensionNamed: className ].

	self readMethodDirectories: classExtensionDirectory forClassDefinition: currentClassExtension inPackage: packageName