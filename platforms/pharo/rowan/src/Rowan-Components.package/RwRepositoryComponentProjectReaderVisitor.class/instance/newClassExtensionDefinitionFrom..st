tonel parser
newClassExtensionDefinitionFrom: anArray
	| className |
	className := (anArray sixth) at: #name.
	currentClassDefinition := nil.
	^currentClassExtension := currentPackageDefinition
		classExtensionDefinitionNamed: className 
		ifAbsent: [ currentPackageDefinition addClassExtensionNamed: className ].