private
_loadedProjectsNamed: someNames

	"Answer a RwProjectSetDefinition containing projects with the given names in the image.
 	If any name is not a loaded project, that name is ignored."

	| loadedProjects |
	loadedProjects := someNames
		collect: [ :aName | Rowan image loadedProjectNamed: aName ifAbsent: [ nil ] ].
	^ RwEntitySet withAll: (loadedProjects select: [ :each | each notNil ])
