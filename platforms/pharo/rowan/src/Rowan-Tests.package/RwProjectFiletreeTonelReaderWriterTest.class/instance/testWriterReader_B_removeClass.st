tests
testWriterReader_B_removeClass

	"https://github.com/GemTalk/Rowan/issues/361"

	"Set of tests that add, change, and remove classes, methods, and extension methods; write to an existing disk repo.
		Expecting to incrementally write only the changed definitions"

	| projectName writtenProjectDefinition readProjectSetDefinition changedProjectSetDefinition visitor
		projectSetModification writeProjectSetDefinition changedProjectDefinition 
		changedProjectSetModification writerVisitorClass writtenPojectSetDefinition  x repositoryRoot |

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

	repositoryRoot := writtenProjectDefinition repositoryRoot.
	self assert: (x := self _classRemovedArtifactFileReference: repositoryRoot) exists.

"copy and make desired modifications"

	changedProjectDefinition := writtenProjectDefinition copy.
	(changedProjectDefinition packageNamed: 'Issue361-Core')
		removeClassNamed: 'Issue361Class1'.

"write changes"
	writerVisitorClass := self _repositoryFormat = 'tonel'
		ifTrue: [ RwModificationTonelWriterVisitor ]
		ifFalse: [ RwModificationFiletreeWriterVisitor ].
	changedProjectSetDefinition:= RwProjectSetDefinition new.
	changedProjectSetDefinition addDefinition: changedProjectDefinition.
	writtenPojectSetDefinition:= RwProjectSetDefinition new.
	writtenPojectSetDefinition addDefinition: writtenProjectDefinition.
	changedProjectSetModification := changedProjectSetDefinition compareAgainstBase: writtenPojectSetDefinition.
	visitor := writerVisitorClass new.

	visitor visit: changedProjectSetModification.

"validation"
	readProjectSetDefinition := writtenProjectDefinition readProjectSet.
	writeProjectSetDefinition := RwProjectSetDefinition new addProject: changedProjectDefinition; yourself.
	projectSetModification := readProjectSetDefinition compareAgainstBase: writeProjectSetDefinition.
	self assert: projectSetModification isEmpty.

	self deny: (self _classRemovedArtifactFileReference: repositoryRoot) exists.