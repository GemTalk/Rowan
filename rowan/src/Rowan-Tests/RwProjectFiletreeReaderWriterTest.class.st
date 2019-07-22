Class {
	#name : 'RwProjectFiletreeReaderWriterTest',
	#superclass : 'RwProjectFiletreeTonelReaderWriterTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwProjectFiletreeReaderWriterTest >> _classExtensionRemovedArtifactFileReference: repositoryRoot [

	^ repositoryRoot / 'rowan' / 'src' / 'Issue361-Extension1.package' / 'Issue361Class1.extension'
]

{ #category : 'private' }
RwProjectFiletreeReaderWriterTest >> _classMethodRemovedArtifactFileReference: repositoryRoot [

	^ repositoryRoot / 'rowan' / 'src' / 'Issue361-Core.package' / 'Issue361Class3.class' / 'class' / 'method10', 'st'
]

{ #category : 'private' }
RwProjectFiletreeReaderWriterTest >> _classRemovedArtifactFileReference: repositoryRoot [

	^ repositoryRoot / 'rowan' / 'src' / 'Issue361-Core.package' / 'Issue361Class1.class'
]

{ #category : 'private' }
RwProjectFiletreeReaderWriterTest >> _instanceMethodRemovedArtifactFileReference: repositoryRoot [

	^ repositoryRoot / 'rowan' / 'src' / 'Issue361-Core.package' / 'Issue361Class3.class' / 'instance' / 'method9', 'st'
]

{ #category : 'private' }
RwProjectFiletreeReaderWriterTest >> _lastClassMethodRemovedArtifactFileReference: repositoryRoot [

	^ repositoryRoot / 'rowan' / 'src' / 'Issue361-Core.package' / 'Issue361Class1.class' / 'class'
]

{ #category : 'private' }
RwProjectFiletreeReaderWriterTest >> _lastInstanceMethodRemovedArtifactFileReference: repositoryRoot [

	^ repositoryRoot / 'rowan' / 'src' / 'Issue361-Core.package' / 'Issue361Class1.class' / 'instance'
]

{ #category : 'private' }
RwProjectFiletreeReaderWriterTest >> _packageRemovedArtifactFileReference: repositoryRoot [

	^ repositoryRoot / 'rowan' / 'src' / 'Issue361-Extension2.package'
]

{ #category : 'private' }
RwProjectFiletreeReaderWriterTest >> _repositoryFormat [

	^ 'filetree'
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
		changedProjectSetModification writerVisitorClass writtenPojectSetDefinition  x repositoryRoot |

	projectName := 'Issue361'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

"write projectDefinition to disk"
	writtenProjectDefinition := self _projectDefinitionForStructureWriters_A: projectName format: self _repositoryFormat.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	writtenProjectDefinition repositoryRoot ensureDeleteAll.
	writtenProjectDefinition create.

	repositoryRoot := writtenProjectDefinition repositoryRoot.
	self assert: (x := self _classMethodRemovedArtifactFileReference: repositoryRoot) exists.
	self assert: (x := self _instanceMethodRemovedArtifactFileReference: repositoryRoot) exists.

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

	self deny: (x := self _classMethodRemovedArtifactFileReference: repositoryRoot) exists.
	self deny: (x := self _instanceMethodRemovedArtifactFileReference: repositoryRoot) exists.
	self assert: (x := self _classMethodRemovedArtifactFileReference: repositoryRoot) parent exists.		"class dir should not be deleted"
	self assert: (x := self _instanceMethodRemovedArtifactFileReference: repositoryRoot) parent exists.	"instance dir should not be deleted"
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
		changedProjectSetModification writerVisitorClass writtenPojectSetDefinition  x repositoryRoot |

	projectName := 'Issue361'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

"write projectDefinition to disk"
	writtenProjectDefinition := self _projectDefinitionForStructureWriters_A: projectName format: self _repositoryFormat.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

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
]
