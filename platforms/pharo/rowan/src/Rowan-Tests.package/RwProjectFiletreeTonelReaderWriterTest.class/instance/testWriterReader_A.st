tests
testWriterReader_A

	"multiple class extensions from multiple packages for multiple classes"

	| projectName writtenProjectDefinition readProjectDefinition readProjectSetDefinition projectSetModification writeProjectSetDefinition |
	projectName := 'Issue361'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].
	(Rowan image projectRepositoryNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :repo | Rowan image _removeProjectRepository: repo ].

"write projectDefinition to disk"
	writtenProjectDefinition := self _projectDefinitionForStructureWriters_A: projectName format: self _repositoryFormat.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].
	(Rowan image projectRepositoryNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :repo | Rowan image _removeProjectRepository: repo ].

	writtenProjectDefinition repositoryRoot ensureDeleteAll.
	writtenProjectDefinition create.

	readProjectDefinition := writtenProjectDefinition copy.
	readProjectSetDefinition := readProjectDefinition readProjectSet.

"validation"
	writeProjectSetDefinition := RwProjectSetDefinition new addProject: writtenProjectDefinition; yourself.
	projectSetModification := readProjectSetDefinition compareAgainstBase: writeProjectSetDefinition.
	self assert: projectSetModification isEmpty.