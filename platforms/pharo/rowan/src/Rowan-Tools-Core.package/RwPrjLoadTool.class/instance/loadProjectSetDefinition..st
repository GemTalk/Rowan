load project definitions
loadProjectSetDefinition: projectSetDefinitionToLoad

	^ self
		loadProjectSetDefinition: projectSetDefinitionToLoad
		instanceMigrator: Rowan platform instanceMigrator
