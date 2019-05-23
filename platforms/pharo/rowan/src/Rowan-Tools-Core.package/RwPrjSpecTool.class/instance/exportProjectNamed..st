smalltalk api
exportProjectNamed: projectName

	| loadedProject |
	loadedProject := Rowan image loadedProjectNamed: projectName.
	self specification: loadedProject specification.
	^ specification export