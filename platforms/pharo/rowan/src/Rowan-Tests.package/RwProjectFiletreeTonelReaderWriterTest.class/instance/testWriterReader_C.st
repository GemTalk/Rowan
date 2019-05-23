tests
testWriterReader_C

	"https://github.com/GemTalk/Rowan/issues/361"

	"write repository to an alternate root directory"

	| projectName writtenProjectDefinition copyProjectDefinition copyProjectSetDefinition 
		projectSetModification writeProjectSetDefinition oldRepositoryRoot newRepositoryRoot |
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

"copy project definition and write to an alternate repository root"
	copyProjectDefinition := writtenProjectDefinition copy.
	oldRepositoryRoot := writtenProjectDefinition repositoryRoot.
	newRepositoryRoot := oldRepositoryRoot parent / (copyProjectDefinition name, '_COPY').
	newRepositoryRoot ensureDeleteAll.
	copyProjectDefinition repositoryRoot: newRepositoryRoot.

	copyProjectDefinition export.

"read from alternate root directory"
	copyProjectSetDefinition := copyProjectDefinition readProjectSet.

"validation"
	writeProjectSetDefinition := RwProjectSetDefinition new addProject: writtenProjectDefinition; yourself.
	projectSetModification := copyProjectSetDefinition compareAgainstBase: writeProjectSetDefinition.
	self assert: projectSetModification isEmpty.