read project definitions
readProjectSetForComponentProjectDefinition: projectComponentDefinition withConfigurations: configNames groupNames: groupNames

	^ self 
		readProjectSetForComponentProjectDefinition: projectComponentDefinition 
			withConfigurations: configNames 
			groupNames: groupNames 
			platformConfigurationAttributes: Rowan platformConfigurationAttributes