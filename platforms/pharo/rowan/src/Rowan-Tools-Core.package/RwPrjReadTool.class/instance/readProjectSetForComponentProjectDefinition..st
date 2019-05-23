read project definitions
readProjectSetForComponentProjectDefinition: projectComponentDefinition

	projectComponentDefinition loadedConfigurationNames
		ifNotNil: [:configNames |
			"project has been loaded previously, use the loaded config and group names"
			^ self 
					readProjectSetForComponentProjectDefinition: projectComponentDefinition
					withConfigurations: configNames
					groupNames: projectComponentDefinition loadedGroupNames ].
	"project has not been loaded previously use the default config and group names from the spec"
	^ self 
			readProjectSetForComponentProjectDefinition: projectComponentDefinition
			withConfigurations: projectComponentDefinition defaultConfigurationNames
			groupNames: projectComponentDefinition defaultGroupNames