deriving
deriveLoadedThings

	"extract the loaded projects that correspond to the project defintions held by the receiver"

	^ RwEntitySet
		withAll:
			((self definitionNames
				collect: [ :projectName | Rowan image loadedProjectNamed: projectName ifAbsent: [  ] ])
				select: [ :each | each notNil ])
