read components
readConfigurationsForProjectComponentDefinition: projectComponentDefinition withConfigurations: configNames groupNames: groupNames platformConfigurationAttributes: platformConfigurationAttributes forLoad: forLoad

	| theConfigNames theGroupNames |
	theConfigNames := configNames isEmpty
		ifTrue: [ projectComponentDefinition defaultConfigurationNames ]
		ifFalse: [ configNames ].
	theGroupNames := groupNames isEmpty
		ifTrue: [  projectComponentDefinition defaultGroupNames ]
		ifFalse: [ groupNames ].
	^ self 
		_visitConfigurations: RwProjectLoadComponentVisitor 
		forProjectComponentDefinition: projectComponentDefinition 
		withConfigurations: theConfigNames 
		groupNames: theGroupNames
		platformConfigurationAttributes: platformConfigurationAttributes
		forLoad: forLoad