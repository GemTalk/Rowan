Class {
	#name : 'RwProjectFiletreeReaderWriterTest',
	#superclass : 'RwProjectFiletreeTonelReaderWriterTest',
	#category : 'Rowan-Obsolete-Tests'
}

{ #category : 'private' }
RwProjectFiletreeReaderWriterTest >> _classExtensionRemovedArtifactFileReference: projectDefinition [

	^ projectDefinition packagesRoot / 'Issue361-Extension1.package' / 'Issue361Class1.extension'
]

{ #category : 'private' }
RwProjectFiletreeReaderWriterTest >> _classMethodRemovedArtifactFileReference: projectDefinition [

	^ projectDefinition packagesRoot / 'Issue361-Core.package' / 'Issue361Class3.class' / 'class' / 'method10', 'st'
]

{ #category : 'private' }
RwProjectFiletreeReaderWriterTest >> _classMovedArtifactFileReference: projectDefinition [

	^ projectDefinition packagesRoot / 'Issue361-Core.package' / 'Issue361Class2.class'
]

{ #category : 'private' }
RwProjectFiletreeReaderWriterTest >> _classRemovedArtifactFileReference: projectDefinition [

	^ projectDefinition packagesRoot / 'Issue361-Core.package' / 'Issue361Class1.class'
]

{ #category : 'private' }
RwProjectFiletreeReaderWriterTest >> _instanceMethodMovedArtifactFileReference: projectDefinition [

	^projectDefinition packagesRoot / 'Issue361-Core.package' / 'Issue361Class3.class' / 'instance' / 'method9', 'st'
]

{ #category : 'private' }
RwProjectFiletreeReaderWriterTest >> _instanceMethodRemovedArtifactFileReference: projectDefinition [

	^ projectDefinition packagesRoot / 'Issue361-Core.package' / 'Issue361Class3.class' / 'instance' / 'method9', 'st'
]

{ #category : 'private' }
RwProjectFiletreeReaderWriterTest >> _lastClassMethodRemovedArtifactFileReference: projectDefinition [

	^ projectDefinition packagesRoot / 'Issue361-Core.package' / 'Issue361Class1.class' / 'class'
]

{ #category : 'private' }
RwProjectFiletreeReaderWriterTest >> _lastInstanceMethodMovedArtifactFileReference: projectDefinition [

	^ projectDefinition packagesRoot / 'Issue361-Core.package' / 'Issue361Class1.class' / 'instance'
]

{ #category : 'private' }
RwProjectFiletreeReaderWriterTest >> _lastInstanceMethodRemovedArtifactFileReference: projectDefinition [

	^ projectDefinition packagesRoot / 'Issue361-Core.package' / 'Issue361Class1.class' / 'instance'
]

{ #category : 'private' }
RwProjectFiletreeReaderWriterTest >> _packageRemovedArtifactFileReference: projectDefinition [

	^ projectDefinition packagesRoot / 'Issue361-Extension2.package'
]

{ #category : 'private' }
RwProjectFiletreeReaderWriterTest >> _repositoryFormat [

	^ 'filetree'
]

{ #category : 'tests' }
RwProjectFiletreeReaderWriterTest >> testWriterReader_B_moveInstanceClassMethods_518 [

	"https://github.com/GemTalk/Rowan/issues/518"

	"Set of tests that add, change, and remove classes, methods, and extension methods; write to an existing disk repo.
		Expecting to incrementally write only the changed definitions"

	"only applies to filetree, since methods are not stored in separate files for tonel"

	"delete instance/class directory when last instance/class method is removed"

	| projectName writtenProjectDefinition readProjectSetDefinition changedProjectSetDefinition visitor
		projectSetModification writeProjectSetDefinition changedProjectDefinition 
		changedProjectSetModification writerVisitorClass writtenPojectSetDefinition  x
		movedMethod |

	projectName := 'Issue361'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

"write projectDefinition to disk"
	writtenProjectDefinition := self _projectDefinitionForStructureWriters_A: projectName format: self _repositoryFormat.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	writtenProjectDefinition repositoryRoot ensureDeleteAll.
	writtenProjectDefinition resolveProject; export.

	self assert: (x := self _instanceMethodMovedArtifactFileReference: writtenProjectDefinition) exists.

"copy and make desired modifications"

	changedProjectDefinition := writtenProjectDefinition copy.
	movedMethod := ((changedProjectDefinition packageNamed: 'Issue361-Core')
		classDefinitionNamed: 'Issue361Class3'ifAbsent: [])
		removeInstanceMethod: #method9.
	((changedProjectDefinition packageNamed: 'Issue361-Core')
		classDefinitionNamed: 'Issue361Class2'ifAbsent: [])
		addInstanceMethodDefinition: movedMethod.

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

	self deny: (x := self _instanceMethodMovedArtifactFileReference: writtenProjectDefinition) exists.
	self assert: (x := self _instanceMethodMovedArtifactFileReference: writtenProjectDefinition) parent exists.	"instance dir should not be deleted"
]

{ #category : 'tests' }
RwProjectFiletreeReaderWriterTest >> testWriterReader_B_moveLastInstanceClassMethods_518 [

	"https://github.com/GemTalk/Rowan/issues/518"

	"Set of tests that add, change, and remove classes, methods, and extension methods; write to an existing disk repo.
		Expecting to incrementally write only the changed definitions"

	"only applies to filetree, since methods are not stored in separate files for tonel"

	"remove the instance or class method file"

	| projectName writtenProjectDefinition readProjectSetDefinition changedProjectSetDefinition visitor
		projectSetModification writeProjectSetDefinition changedProjectDefinition 
		changedProjectSetModification writerVisitorClass writtenPojectSetDefinition  x movedMethod |

	projectName := 'Issue361'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

"write projectDefinition to disk"
	writtenProjectDefinition := self _projectDefinitionForStructureWriters_A: projectName format: self _repositoryFormat.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	writtenProjectDefinition repositoryRoot ensureDeleteAll.
	writtenProjectDefinition resolveProject; export.

	self assert: (x := self _lastInstanceMethodMovedArtifactFileReference: writtenProjectDefinition) exists.

"copy and make desired modifications"

	changedProjectDefinition := writtenProjectDefinition copy.
	movedMethod := ((changedProjectDefinition packageNamed: 'Issue361-Core')
		classDefinitionNamed: 'Issue361Class1'ifAbsent: [])
		removeInstanceMethod: #method6.
	((changedProjectDefinition packageNamed: 'Issue361-Core')
		classDefinitionNamed: 'Issue361Class2'ifAbsent: [])
		addInstanceMethodDefinition: movedMethod.

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

	self deny: (x := self _lastInstanceMethodMovedArtifactFileReference: writtenProjectDefinition) exists.
]

{ #category : 'tests' }
RwProjectFiletreeReaderWriterTest >> testWriterReader_B_removeInstanceClassMethods [

	"https://github.com/GemTalk/Rowan/issues/361"

	"Set of tests that add, change, and remove classes, methods, and extension methods; write to an existing disk repo.
		Expecting to incrementally write only the changed definitions"

	"only applies to filetree, since methods are not stored in separate files for tonel"

	"delete instance/class directory when last instance/class method is removed"

	| projectName writtenProjectDefinition readProjectSetDefinition changedProjectSetDefinition visitor
		projectSetModification writeProjectSetDefinition changedProjectDefinition 
		changedProjectSetModification writerVisitorClass writtenPojectSetDefinition  x |

	projectName := 'Issue361'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

"write projectDefinition to disk"
	writtenProjectDefinition := self _projectDefinitionForStructureWriters_A: projectName format: self _repositoryFormat.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	writtenProjectDefinition repositoryRoot ensureDeleteAll.
	writtenProjectDefinition resolveProject; export.

	self assert: (x := self _classMethodRemovedArtifactFileReference: writtenProjectDefinition) exists.
	self assert: (x := self _instanceMethodRemovedArtifactFileReference: writtenProjectDefinition) exists.

"copy and make desired modifications"

	changedProjectDefinition := writtenProjectDefinition copy.
	((changedProjectDefinition packageNamed: 'Issue361-Core')
		classDefinitionNamed: 'Issue361Class3'ifAbsent: [])
		removeInstanceMethod: #method9;
		removeClassMethod: #method10.

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

	self deny: (x := self _classMethodRemovedArtifactFileReference: writtenProjectDefinition) exists.
	self deny: (x := self _instanceMethodRemovedArtifactFileReference: writtenProjectDefinition) exists.
	self assert: (x := self _classMethodRemovedArtifactFileReference: writtenProjectDefinition) parent exists.		"class dir should not be deleted"
	self assert: (x := self _instanceMethodRemovedArtifactFileReference: writtenProjectDefinition) parent exists.	"instance dir should not be deleted"
]

{ #category : 'tests' }
RwProjectFiletreeReaderWriterTest >> testWriterReader_B_removeLastInstanceClassMethods [

	"https://github.com/GemTalk/Rowan/issues/361"

	"Set of tests that add, change, and remove classes, methods, and extension methods; write to an existing disk repo.
		Expecting to incrementally write only the changed definitions"

	"only applies to filetree, since methods are not stored in separate files for tonel"

	"remove the instance or class method file"

	| projectName writtenProjectDefinition readProjectSetDefinition changedProjectSetDefinition visitor
		projectSetModification writeProjectSetDefinition changedProjectDefinition 
		changedProjectSetModification writerVisitorClass writtenPojectSetDefinition  x |

	projectName := 'Issue361'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

"write projectDefinition to disk"
	writtenProjectDefinition := self _projectDefinitionForStructureWriters_A: projectName format: self _repositoryFormat.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	writtenProjectDefinition repositoryRoot ensureDeleteAll.
	writtenProjectDefinition resolveProject; export.

	self assert: (x := self _lastClassMethodRemovedArtifactFileReference: writtenProjectDefinition) exists.
	self assert: (x := self _lastInstanceMethodRemovedArtifactFileReference: writtenProjectDefinition) exists.

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

	self deny: (x := self _lastClassMethodRemovedArtifactFileReference: writtenProjectDefinition) exists.
	self deny: (x := self _lastInstanceMethodRemovedArtifactFileReference: writtenProjectDefinition) exists.
]
