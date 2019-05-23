smalltalk api
patchesForProjectNamed: projectName

	| imageProject |
	imageProject := Rowan image loadedProjectNamed: projectName.
	^ self patchesForProjectDefinition: imageProject asDefinition