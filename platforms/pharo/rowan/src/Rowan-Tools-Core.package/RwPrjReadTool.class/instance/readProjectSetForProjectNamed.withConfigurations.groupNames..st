read project by name
readProjectSetForProjectNamed: projectName withConfigurations: configNames groupNames: groupNames

	| projectDefinition |
	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	^ projectDefinition
		readProjectSetReadTool: self
			withConfigurations: configNames 
			groupNames: groupNames