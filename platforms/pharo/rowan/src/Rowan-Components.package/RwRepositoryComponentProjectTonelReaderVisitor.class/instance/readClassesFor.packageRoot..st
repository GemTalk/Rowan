class reading
readClassesFor: packageName packageRoot: packageRoot

	| classFileExtensions classExtensionFileExtensions |
	currentPackageDefinition := currentProjectDefinition 
		packageNamed: packageName 
		ifAbsent: [ currentProjectDefinition addPackageNamed: packageName ].
	classExtensionFileExtensions := self classExtensionFileExtensions.
	classFileExtensions := self classFileExtensions.
	packageRoot files do: [:file |
		| fileExtensions |
		fileExtensions := file extensions asArray.
		fileExtensions = classFileExtensions
			ifTrue: [ self readClassFile: file inPackage: packageName ]
			ifFalse: [
				fileExtensions = classExtensionFileExtensions
					ifTrue: [ self readClassExtensionFile: file  inPackage: packageName ] ] ]