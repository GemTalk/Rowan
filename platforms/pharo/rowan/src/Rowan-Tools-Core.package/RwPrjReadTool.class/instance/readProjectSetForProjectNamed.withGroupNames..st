read project by name
readProjectSetForProjectNamed: projectName withGroupNames: groupNames

	| projectDefinition theConfigNames |
	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	theConfigNames := projectDefinition loadedConfigurationNames
		ifNotNil: [:configNames | configNames ]
		ifNil: [
			projectDefinition defaultConfigurationNames
				ifNotNil: [:configNames | configNames ]
				ifNil: [ #('Default') ] ].
	^ projectDefinition
		readProjectSetReadTool: self
			withConfigurations: theConfigNames 
			groupNames: groupNames