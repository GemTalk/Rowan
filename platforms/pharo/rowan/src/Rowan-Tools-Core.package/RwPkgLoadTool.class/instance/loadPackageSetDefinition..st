smalltalk api
loadPackageSetDefinition: packageSetDefinitionToLoad

	^ self
		loadPackageSetDefinition: packageSetDefinitionToLoad
		instanceMigrator: Rowan platform instanceMigrator