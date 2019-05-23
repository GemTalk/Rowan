tests
testWriterReader_B_removeLastInstanceClassMethods

	"https://github.com/GemTalk/Rowan/issues/361"

	"Set of tests that add, change, and remove classes, methods, and extension methods; write to an existing disk repo.
		Expecting to incrementally write only the changed definitions"

	"only applies to filetree, since methods are not stored in separate files for tonel"

	"remove the instance or class method file"

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
	self assert: (x := self _lastClassMethodRemovedArtifactFileReference: repositoryRoot) exists.
	self assert: (x := self _lastInstanceMethodRemovedArtifactFileReference: repositoryRoot) exists.

"copy and make desired modifications"

	changedProjectDefinition := writtenProjectDefinition copy.
	((changedProjectDefinition packageNamed: 'Issue361-Core')
		classDefinitionNamed: 'Issue361Class1'ifAbsent: [])
		removeInstanceMethod: #method6;
		removeClassMethod: #method2.

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

	self deny: (x := self _lastClassMethodRemovedArtifactFileReference: repositoryRoot) exists.
	self deny: (x := self _lastInstanceMethodRemovedArtifactFileReference: repositoryRoot) exists.