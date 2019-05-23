smalltalk api
commitLogProjectNamed: projectName limit: logLimit

	^ (Rowan image loadedProjectNamed: projectName) commitLog: logLimit