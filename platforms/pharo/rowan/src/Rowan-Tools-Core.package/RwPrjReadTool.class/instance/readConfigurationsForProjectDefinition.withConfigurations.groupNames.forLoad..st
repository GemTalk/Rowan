read configurations
readConfigurationsForProjectDefinition: projectDefinition withConfigurations: configNames groupNames: groupNames forLoad: forLoad

	| theConfigNames theGroupNames |
	theConfigNames := configNames isEmpty
		ifTrue: [ projectDefinition defaultConfigurationNames ]
		ifFalse: [ configNames ].
	theGroupNames := groupNames isEmpty
		ifTrue: [  projectDefinition defaultGroupNames ]
		ifFalse: [ groupNames ].
	^ self 
		_visitConfigurations: RwProjectLoadConfigurationVisitor 
		forProjectDefinition: projectDefinition 
		withConfigurations: theConfigNames 
		groupNames: theGroupNames
		platformConfigurationAttributes: Rowan platformConfigurationAttributes
		forLoad: forLoad