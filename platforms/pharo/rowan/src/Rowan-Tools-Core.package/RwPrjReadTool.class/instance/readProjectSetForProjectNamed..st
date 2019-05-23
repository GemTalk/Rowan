read project by name
readProjectSetForProjectNamed: projectName

	^ (Rowan image loadedProjectNamed: projectName) asDefinition readProjectSet