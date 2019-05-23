smalltalk api
specForProjectNamed: projectName

	| loadedProject |
	loadedProject := Rowan image loadedProjectNamed: projectName.
	^ loadedProject specification