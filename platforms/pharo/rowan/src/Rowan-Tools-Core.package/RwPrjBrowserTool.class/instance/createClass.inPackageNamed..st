class browsing
createClass: classDefinition inPackageNamed: packageName

	| loadedPackage projectDefinition projectTools |
	loadedPackage := Rowan image
		loadedPackageNamed: packageName
		ifAbsent: [ self error: 'The package named ' , packageName printString , ' was not found' ].
	projectDefinition := loadedPackage loadedProject asDefinition.

	projectTools := Rowan projectTools.

	projectTools edit
		addClass: classDefinition
		inPackageNamed: packageName
		inProject: projectDefinition.

	projectTools load loadProjectDefinition: projectDefinition