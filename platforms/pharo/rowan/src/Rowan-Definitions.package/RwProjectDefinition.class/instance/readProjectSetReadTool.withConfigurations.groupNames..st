reading
readProjectSetReadTool: readTool withConfigurations: theConfigNames groupNames: theGroupNames

	^ readTool
		readProjectSetForProjectDefinition: self
			withConfigurations: theConfigNames 
			groupNames: theGroupNames