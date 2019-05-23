read components
readConfigurationsForProjectComponentDefinition: projectDefinition withConfigurations: configNames groupNames: groupNames platformConfigurationAttributes: platformConfigurationAttributes

	^self 
		readConfigurationsForProjectComponentDefinition: projectDefinition 
			withConfigurations: configNames 
			groupNames: groupNames 
			platformConfigurationAttributes: platformConfigurationAttributes 
			forLoad: true