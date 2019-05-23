reading
readProjectSetReadTool: readTool withConfigurations: theConfigNames groupNames: theGroupNames

	^ readTool
		readProjectSetForComponentProjectDefinition: self
			withConfigurations: theConfigNames 
			groupNames: theGroupNames