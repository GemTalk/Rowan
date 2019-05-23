package browsing
removePackageNamed: packageName

	| loadedPackage projectDef |
	loadedPackage := self _loadedPackageNamed: packageName.
	projectDef := loadedPackage loadedProject asDefinition.
	projectDef removePackageNamed: loadedPackage name.

	Rowan projectTools load loadProjectDefinition: projectDef
