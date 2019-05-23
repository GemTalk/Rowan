running
tearDown
	toDelete
		select: [ :aFileRef | aFileRef exists ]
		thenDo: [ :aFileRef | aFileRef deleteAll ].
	projectsToDelete
		do: [ :projectName | 
		(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
			ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].
			(Rowan image projectRepositoryNamed: projectName ifAbsent: [  ])
				ifNotNil: [ :repo | Rowan image _removeProjectRepository: repo ] ].
	super tearDown