class reading
readClassesFor: packageName packageRoot: packageRoot

	currentPackageDefinition := currentProjectDefinition 
		packageNamed: packageName 
		ifAbsent: [ currentProjectDefinition addPackageNamed: packageName ].
	packageRoot directories do: [:directory |
		| directoryExtension |
		directoryExtension := directory extension.
		directoryExtension = 'class'
			ifTrue: [ self readClassDirectory: directory inPackage: packageName ]
			ifFalse: [
				directoryExtension = 'extension'
					ifTrue: [ self readClassExtensionDirectory: directory  inPackage: packageName ] ] ]