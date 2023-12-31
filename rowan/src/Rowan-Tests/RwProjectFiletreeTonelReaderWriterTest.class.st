Class {
	#name : 'RwProjectFiletreeTonelReaderWriterTest',
	#superclass : 'RwProjectReaderWriterTest',
	#category : 'Rowan-Tests'
}

{ #category : 'testing' }
RwProjectFiletreeTonelReaderWriterTest class >> isAbstract [
  "Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

  ^ self sunitName = #'RwProjectFiletreeTonelReaderWriterTest'
]

{ #category : 'private' }
RwProjectFiletreeTonelReaderWriterTest >> _classExtensionRemovedArtifactFileReference: projectDefinition [

	self subclassResponsibility: #_classExtensionRemovedArtifactFileReference:
]

{ #category : 'private' }
RwProjectFiletreeTonelReaderWriterTest >> _classMovedArtifactFileReference: projectDefinition [

	self subclassResponsibility: #_classMovedArtifactFileReference:
]

{ #category : 'private' }
RwProjectFiletreeTonelReaderWriterTest >> _classRemovedArtifactFileReference: projectDefinition [

	self subclassResponsibility: #_classRemovedArtifactFileReference:
]

{ #category : 'private' }
RwProjectFiletreeTonelReaderWriterTest >> _packageRemovedArtifactFileReference: projectDefinition [

	self subclassResponsibility: #_packageRemovedArtifactFileReference:
]

{ #category : 'private' }
RwProjectFiletreeTonelReaderWriterTest >> _repositoryFormat [

	^ self subclassResponsibility: #_repositoryFormat
]

{ #category : 'private' }
RwProjectFiletreeTonelReaderWriterTest >> _validateIssue122ProjectDefinitionSet: projectDefinitionSet projectName: projectName [

	self assert: projectDefinitionSet  projects size = 1.
	projectDefinitionSet  projects keysAndValuesDo: [:projName :projectDefinition |  
		self assert: projectDefinition name = projectName.
		self assert: projectDefinition packages size = 2.
		projectDefinition  packages keysAndValuesDo: [:packageName :packageDefinition |
			packageDefinition name = 'Issue122-Core'
				ifTrue: [
					self assert: packageDefinition classExtensions isEmpty.
					self assert: packageDefinition classDefinitions size = 3.
					packageDefinition classDefinitions keysAndValuesDo: [:className :classDefinition | 
						classDefinition name = 'Issue122Class1'
							ifTrue: [
								self assert: classDefinition category = packageName.
								self assert: classDefinition instanceMethodDefinitions size = 1.
								self assert: classDefinition classMethodDefinitions size = 1 ]
							ifFalse: [
								classDefinition name = 'Issue122Class2'
								ifTrue: [
									self assert: classDefinition category = packageName.
									self assert: classDefinition instanceMethodDefinitions size = 1.
									self assert: classDefinition classMethodDefinitions size = 0 ]
								ifFalse: [
									classDefinition name = 'Issue122Class3'
									ifTrue: [
										self assert: classDefinition instanceMethodDefinitions size = 1.
										self assert: classDefinition category = packageName.
										self assert: classDefinition classMethodDefinitions size = 1 ]
									ifFalse: [ self assert: false description: 'unexpected class definition ', classDefinition name printString ] ] ] ] ]
				ifFalse: [
					packageDefinition name = 'Issue122-Extension1'
						ifTrue: [
							self assert: packageDefinition classDefinitions isEmpty.
							self assert: packageDefinition classExtensions size = 1.
							packageDefinition classExtensions keysAndValuesDo: [:className :classExtension | 
								classExtension name = 'Issue122Class1'
									ifTrue: [ 
										self assert: classExtension classMethodDefinitions isEmpty.
										self assert: classExtension instanceMethodDefinitions size = 2.
										classExtension instanceMethodDefinitions keysAndValuesDo: [:selector :methodDefinition | 
											selector = #method1
												ifTrue: [	self assert: methodDefinition protocol = '*issue122-extension1' ]
												ifFalse: [
													selector = #method122
														ifTrue: [	self assert: methodDefinition protocol = '*issue122-extension1' ]
														ifFalse: [ self assert: false description: 'unexpected method definition ', methodDefinition selector printString ] ] ] ]
									ifFalse: [ self assert: false description: 'unexpected classExtenstion definition ', classExtension name printString ] ] ]
						ifFalse: [ self assert: false description: 'unexpected package definition ', packageDefinition name printString ] ] ] ]
]

{ #category : 'private' }
RwProjectFiletreeTonelReaderWriterTest >> _validateIssue122Repaired_CategoryValidationError_ProjectDefinitionSet: projectDefinitionSet projectName: projectName [

	self assert: projectDefinitionSet  projects size = 1.
	projectDefinitionSet  projects keysAndValuesDo: [:projName :projectDefinition |  
		self assert: projectDefinition name = projectName.
		self assert: projectDefinition packages size = 1.
		projectDefinition  packages keysAndValuesDo: [:packageName :packageDefinition |
			packageDefinition name = 'Issue122-Core-CategoryValidationError'
				ifTrue: [
					self assert: packageDefinition classExtensions isEmpty.
					self assert: packageDefinition classDefinitions size = 1.
					packageDefinition classDefinitions keysAndValuesDo: [:className :classDefinition | 
						classDefinition name = 'Issue122Class1'
							ifTrue: [
								self assert: classDefinition category = packageName.
								self assert: classDefinition instanceMethodDefinitions size = 1.
								self assert: classDefinition classMethodDefinitions size = 1 ]
							ifFalse: [  self assert: false description: 'unexpected class definition ', classDefinition name printString  ] ] ]
				ifFalse: [ self assert: false description: 'unexpected package definition ', packageDefinition name printString ] ] ]
]

{ #category : 'private' }
RwProjectFiletreeTonelReaderWriterTest >> _validateIssue122Repaired_CoreProtocolValidationError_ProjectDefinitionSet: projectDefinitionSet projectName: projectName [

	self assert: projectDefinitionSet  projects size = 1.
	projectDefinitionSet  projects keysAndValuesDo: [:projName :projectDefinition |  
		self assert: projectDefinition name = projectName.
		self assert: projectDefinition packages size = 1.
		projectDefinition  packages keysAndValuesDo: [:packageName :packageDefinition |
			packageDefinition name = 'Issue122-Core-ProtocolValidationError'
				ifTrue: [
					self assert: packageDefinition classExtensions isEmpty.
					self assert: packageDefinition classDefinitions size = 1.
					packageDefinition classDefinitions keysAndValuesDo: [:className :classDefinition | 
						classDefinition name = 'Issue122Class1'
							ifTrue: [
								self assert: classDefinition category = packageName.
								self assert: classDefinition instanceMethodDefinitions size = 1.
								self assert: classDefinition classMethodDefinitions size = 1.
								classDefinition instanceMethodDefinitions keysAndValuesDo: [:selector :methodDefinition | 
									self assert: selector = #method6.
									self assert: methodDefinition protocol = 'repaired' ] ]
							ifFalse: [  self assert: false description: 'unexpected class definition ', classDefinition name printString  ] ] ]
				ifFalse: [ self assert: false description: 'unexpected package definition ', packageDefinition name printString ] ] ]
]

{ #category : 'private' }
RwProjectFiletreeTonelReaderWriterTest >> _validateIssue122Repaired_ExtensionProtocolValidationError_ProjectDefinitionSet: projectDefinitionSet projectName: projectName [


	self assert: projectDefinitionSet  projects size = 1.
	projectDefinitionSet  projects keysAndValuesDo: [:projName :projectDefinition |  
		self assert: projectDefinition name = projectName.
		self assert: projectDefinition packages size = 2.
		projectDefinition  packages keysAndValuesDo: [:packageName :packageDefinition |
			packageDefinition name = 'Issue122-Core'
				ifTrue: [
					self assert: packageDefinition classExtensions isEmpty.
					self assert: packageDefinition classDefinitions size = 3.
					packageDefinition classDefinitions keysAndValuesDo: [:className :classDefinition | 
						classDefinition name = 'Issue122Class1'
							ifTrue: [
								self assert: classDefinition category = packageName.
								self assert: classDefinition instanceMethodDefinitions size = 1.
								self assert: classDefinition classMethodDefinitions size = 1 ]
							ifFalse: [
								classDefinition name = 'Issue122Class2'
								ifTrue: [
									self assert: classDefinition category = packageName.
									self assert: classDefinition instanceMethodDefinitions size = 1.
									self assert: classDefinition classMethodDefinitions size = 0 ]
								ifFalse: [
									classDefinition name = 'Issue122Class3'
									ifTrue: [
										self assert: classDefinition instanceMethodDefinitions size = 1.
										self assert: classDefinition category = packageName.
										self assert: classDefinition classMethodDefinitions size = 1 ]
									ifFalse: [ self assert: false description: 'unexpected class definition ', classDefinition name printString ] ] ] ] ]
				ifFalse: [
					packageDefinition name = 'Issue122-Extension1-ProtocolValidationError'
						ifTrue: [
							self assert: packageDefinition classDefinitions isEmpty.
							self assert: packageDefinition classExtensions size = 1.
							packageDefinition classExtensions keysAndValuesDo: [:className :classExtension | 
								classExtension name = 'Issue122Class1'
									ifTrue: [ 
										self assert: classExtension classMethodDefinitions size = 0.
										self assert: classExtension instanceMethodDefinitions size = 2.
										classExtension instanceMethodDefinitions keysAndValuesDo: [:selector :methodDefinition | 
											selector = #method1
												ifTrue: [	self assert: methodDefinition protocol = '*issue122-extension1-protocolvalidationerror' ]
												ifFalse: [
													selector = #method20
														ifTrue: [	self assert: methodDefinition protocol = '*issue122-extension1-protocolvalidationerror' ]
														ifFalse: [ self assert: false description: 'unexpected method definition ', methodDefinition selector printString ] ] ] ]
											ifFalse: [ self assert: false description: 'unexpected classExtenstion definition ', classExtension name printString ] ] ]
						ifFalse: [ self assert: false description: 'unexpected package definition ', packageDefinition name printString ] ] ] ]
]

{ #category : 'private' }
RwProjectFiletreeTonelReaderWriterTest >> _validateIssue361ProjectDefinitionSet: projectDefinitionSet projectName: projectName [

	"on disk the class extions directory/file exists, but is empty ... need to properly read from disk without errors"

	self assert: projectDefinitionSet  projects size = 1.
	projectDefinitionSet  projects keysAndValuesDo: [:projName :projectDefinition |  
		self assert: projectDefinition name = projectName.
		self assert: projectDefinition packages size = 2.
		projectDefinition  packages keysAndValuesDo: [:packageName :packageDefinition |
			packageDefinition name = 'Issue122-Core'
				ifTrue: [
					self assert: packageDefinition classExtensions isEmpty.
					self assert: packageDefinition classDefinitions size = 3.
					packageDefinition classDefinitions keysAndValuesDo: [:className :classDefinition | 
						classDefinition name = 'Issue122Class1'
							ifTrue: [
								self assert: classDefinition category = packageName.
								self assert: classDefinition instanceMethodDefinitions size = 1.
								self assert: classDefinition classMethodDefinitions size = 1 ]
							ifFalse: [
								classDefinition name = 'Issue122Class2'
								ifTrue: [
									self assert: classDefinition category = packageName.
									self assert: classDefinition instanceMethodDefinitions size = 1.
									self assert: classDefinition classMethodDefinitions size = 0 ]
								ifFalse: [
									classDefinition name = 'Issue122Class3'
									ifTrue: [
										self assert: classDefinition instanceMethodDefinitions size = 1.
										self assert: classDefinition category = packageName.
										self assert: classDefinition classMethodDefinitions size = 1 ]
									ifFalse: [ self assert: false description: 'unexpected class definition ', classDefinition name printString ] ] ] ] ]
				ifFalse: [
					packageDefinition name = 'Issue122-Extension1'
						ifTrue: [
							self assert: packageDefinition classDefinitions isEmpty.
							self assert: packageDefinition classExtensions size = 1.
							packageDefinition classExtensions keysAndValuesDo: [:className :classExtension | 
								classExtension name = 'Issue122Class1'
									ifTrue: [ 
										self assert: classExtension instanceMethodDefinitions isEmpty.
										self assert: classExtension classMethodDefinitions isEmpty ]
									ifFalse: [ self assert: false description: 'unexpected classExtenstion definition ', classExtension name printString ] ] ]
						ifFalse: [ self assert: false description: 'unexpected package definition ', packageDefinition name printString ] ] ] ]
]

{ #category : 'tests' }
RwProjectFiletreeTonelReaderWriterTest >> testWriterReader_A [

	"multiple class extensions from multiple packages for multiple classes"

	| projectName writtenProjectDefinition readProjectDefinition readProjectSetDefinition projectSetModification writeProjectSetDefinition |
	projectName := 'Issue361'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

"write projectDefinition to disk"
	writtenProjectDefinition := self _projectDefinitionForStructureWriters_A: projectName format: self _repositoryFormat.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	writtenProjectDefinition repositoryRoot ensureDeleteAll.
	writtenProjectDefinition resolveProject; export.

	readProjectDefinition := writtenProjectDefinition copy.
	readProjectSetDefinition := readProjectDefinition readProjectSet.

"validation"
	writeProjectSetDefinition := RwProjectSetDefinition new addProject: writtenProjectDefinition; yourself.
	projectSetModification := readProjectSetDefinition compareAgainstBase: writeProjectSetDefinition.
	self assert: projectSetModification isEmpty.
]

{ #category : 'tests' }
RwProjectFiletreeTonelReaderWriterTest >> testWriterReader_B_moveClass_518 [

	"https://github.com/GemTalk/Rowan/issues/128"

	"Set of tests that add, change, and remove classes, methods, and extension methods; write to an existing disk repo.
		Expecting to incrementally write only the changed definitions"

	| projectName writtenProjectDefinition readProjectSetDefinition changedProjectSetDefinition visitor
		projectSetModification writeProjectSetDefinition changedProjectDefinition 
		changedProjectSetModification writerVisitorClass writtenPojectSetDefinition  x  
		movedClassDef |

	projectName := 'Issue361'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

"write projectDefinition to disk"
	writtenProjectDefinition := self _projectDefinitionForStructureWriters_A: projectName format: self _repositoryFormat.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	writtenProjectDefinition repositoryRoot ensureDeleteAll.
	writtenProjectDefinition resolveProject; export.

	self assert: (x := self _classRemovedArtifactFileReference: writtenProjectDefinition) exists.

"copy and make desired modifications"

	changedProjectDefinition := writtenProjectDefinition copy.
	movedClassDef := (changedProjectDefinition packageNamed: 'Issue361-Core')
		removeClassNamed: 'Issue361Class2'.
	movedClassDef category:  'Issue361-Extension1'.
	(changedProjectDefinition packageNamed: 'Issue361-Extension1')
		addClassDefinition: movedClassDef.

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

	self deny: (self _classMovedArtifactFileReference: writtenProjectDefinition) exists.
]

{ #category : 'tests' }
RwProjectFiletreeTonelReaderWriterTest >> testWriterReader_B_removeClass [

	"https://github.com/GemTalk/Rowan/issues/361"

	"Set of tests that add, change, and remove classes, methods, and extension methods; write to an existing disk repo.
		Expecting to incrementally write only the changed definitions"

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

	self assert: (x := self _classRemovedArtifactFileReference: writtenProjectDefinition) exists.

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

	self deny: (self _classRemovedArtifactFileReference: writtenProjectDefinition) exists.
]

{ #category : 'tests' }
RwProjectFiletreeTonelReaderWriterTest >> testWriterReader_B_removeExtensionClass [

	"https://github.com/GemTalk/Rowan/issues/361"

	"Set of tests that add, change, and remove classes, methods, and extension methods; write to an existing disk repo.
		Expecting to incrementally write only the changed definitions"

	| projectName writtenProjectDefinition readProjectSetDefinition changedProjectSetDefinition visitor
		projectSetModification writeProjectSetDefinition changedProjectDefinition changedProjectSetModification
		writerVisitorClass writtenPojectSetDefinition x |

	projectName := 'Issue361'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

"write projectDefinition to disk"
	writtenProjectDefinition := self _projectDefinitionForStructureWriters_A: projectName format: self _repositoryFormat.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	writtenProjectDefinition repositoryRoot ensureDeleteAll.
	writtenProjectDefinition resolveProject; export.

	self assert: (x := self _classExtensionRemovedArtifactFileReference: writtenProjectDefinition) exists.

"copy and make desired modifications"

	changedProjectDefinition := writtenProjectDefinition copy.
	(changedProjectDefinition packageNamed: 'Issue361-Extension1')
		removeClassExtensionDefinitionNamed: 'Issue361Class1'.

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
	projectSetModification := writeProjectSetDefinition compareAgainstBase: readProjectSetDefinition.
	self assert: projectSetModification isEmpty.

	self deny: (self _classExtensionRemovedArtifactFileReference: writtenProjectDefinition) exists
]

{ #category : 'tests' }
RwProjectFiletreeTonelReaderWriterTest >> testWriterReader_B_removePackage [

	"https://github.com/GemTalk/Rowan/issues/361"

	"Set of tests that add, change, and remove classes, methods, and extension methods; write to an existing disk repo.
		Expecting to incrementally write only the changed definitions"

	| projectName writtenProjectDefinition readProjectSetDefinition changedProjectSetDefinition visitor
		projectSetModification writeProjectSetDefinition changedProjectDefinition 
		changedProjectSetModification writerVisitorClass writtenPojectSetDefinition x |

	projectName := 'Issue361'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

"write projectDefinition to disk"
	writtenProjectDefinition := self _projectDefinitionForStructureWriters_A: projectName format: self _repositoryFormat.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	writtenProjectDefinition repositoryRoot ensureDeleteAll.
	writtenProjectDefinition resolveProject; export.

	self assert: (x := self _packageRemovedArtifactFileReference: writtenProjectDefinition) exists.

"copy and make desired modifications"

	changedProjectDefinition := writtenProjectDefinition copy.
	changedProjectDefinition removePackageNamed: 'Issue361-Extension2'.

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
	[readProjectSetDefinition := writtenProjectDefinition readProjectSet]
		on: RwUnreadPackagesErrorNotification
		do: [:ex | ex resume ].
	writeProjectSetDefinition := RwProjectSetDefinition new addProject: changedProjectDefinition; yourself.
	projectSetModification := readProjectSetDefinition compareAgainstBase: writeProjectSetDefinition.
	self assert: projectSetModification isEmpty.

	self deny: (self _packageRemovedArtifactFileReference: writtenProjectDefinition) exists.
]

{ #category : 'tests' }
RwProjectFiletreeTonelReaderWriterTest >> testWriterReader_C [

	"https://github.com/GemTalk/Rowan/issues/361"

	"write repository to an alternate root directory"

	| projectName writtenProjectDefinition copyProjectDefinition copyProjectSetDefinition 
		projectSetModification writeProjectSetDefinition oldRepositoryRoot newRepositoryRoot |
	projectName := 'Issue361'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

"write projectDefinition to disk"
	writtenProjectDefinition := self _projectDefinitionForStructureWriters_A: projectName format: self _repositoryFormat.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	writtenProjectDefinition repositoryRoot ensureDeleteAll.
	writtenProjectDefinition resolveProject; export.

"copy project definition and write to an alternate repository root"
	copyProjectDefinition := writtenProjectDefinition copy.
	oldRepositoryRoot := writtenProjectDefinition repositoryRoot.
	newRepositoryRoot := oldRepositoryRoot parent / (copyProjectDefinition name, '_COPY').
	newRepositoryRoot ensureDeleteAll.
	copyProjectDefinition diskRepositoryRoot: newRepositoryRoot pathString.

	copyProjectDefinition export.

"read from alternate root directory"
	copyProjectSetDefinition := copyProjectDefinition readProjectSet.

"validation"
	writeProjectSetDefinition := RwProjectSetDefinition new addProject: writtenProjectDefinition; yourself.
	projectSetModification := copyProjectSetDefinition compareAgainstBase: writeProjectSetDefinition.
	self assert: projectSetModification isEmpty.
]

{ #category : 'tests' }
RwProjectFiletreeTonelReaderWriterTest >> testWriterReader_D_changeClass [

	"https://github.com/GemTalk/Rowan/issues/361"

	"Set of tests that add, change, and remove classes, methods, and extension methods; write to an existing disk repo.
		Expecting to incrementally write only the changed definitions"

	| projectName writtenProjectDefinition readProjectSetDefinition changedProjectSetDefinition visitor
		projectSetModification writeProjectSetDefinition changedProjectDefinition 
		changedProjectSetModification writerVisitorClass writtenPojectSetDefinition
		classDef packageDef |

	projectName := 'Issue361'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

"write projectDefinition to disk"
	writtenProjectDefinition := self _projectDefinitionForStructureWriters_A: projectName format: self _repositoryFormat.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	writtenProjectDefinition repositoryRoot ensureDeleteAll.
	writtenProjectDefinition resolveProject; export.

"copy and make desired modifications"

	changedProjectDefinition := writtenProjectDefinition copy.
	packageDef := changedProjectDefinition packageNamed: 'Issue361-Core'.
	classDef := packageDef classDefinitionNamed: 'Issue361Class1'.
	classDef instVarNames: classDef instVarNames, #( 'iv1').
	packageDef updateClassDefinition: classDef.

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

	self deny: changedProjectSetModification isEmpty.
	visitor visit: changedProjectSetModification.

"validation"
	readProjectSetDefinition := writtenProjectDefinition readProjectSet.
	writeProjectSetDefinition := RwProjectSetDefinition new addProject: changedProjectDefinition; yourself.
	projectSetModification := readProjectSetDefinition compareAgainstBase: writeProjectSetDefinition.
	self assert: projectSetModification isEmpty.
]

{ #category : 'tests' }
RwProjectFiletreeTonelReaderWriterTest >> testWriterReader_D_changeMethods [

	"https://github.com/GemTalk/Rowan/issues/361"

	"Set of tests that add, change, and remove classes, methods, and extension methods; write to an existing disk repo.
		Expecting to incrementally write only the changed definitions"

	| projectName writtenProjectDefinition readProjectSetDefinition changedProjectSetDefinition visitor
		projectSetModification writeProjectSetDefinition changedProjectDefinition 
		changedProjectSetModification writerVisitorClass writtenPojectSetDefinition
		classDef packageDef |

	projectName := 'Issue361'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

"write projectDefinition to disk"
	writtenProjectDefinition := self _projectDefinitionForStructureWriters_A: projectName format: self _repositoryFormat.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	writtenProjectDefinition repositoryRoot ensureDeleteAll.
	writtenProjectDefinition resolveProject; export.

"copy and make desired modifications"

	changedProjectDefinition := writtenProjectDefinition copy.
	packageDef := changedProjectDefinition packageNamed: 'Issue361-Core'.
	classDef := packageDef classDefinitionNamed: 'Issue361Class1'.
	classDef 
		updateInstanceMethodDefinition: 
			(RwMethodDefinition
					newForSelector: #'method6'
					protocol: 'instance accessing'
					source: 'method6 "changed" ^6');
		updateClassMethodDefinition: 
			(RwMethodDefinition
					newForSelector: #'method2'
					protocol: 'class accessing'
					source: 'method2 "changed" ^2').

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

	self deny: changedProjectSetModification isEmpty.
	visitor visit: changedProjectSetModification.

"validation"
	readProjectSetDefinition := writtenProjectDefinition readProjectSet.
	writeProjectSetDefinition := RwProjectSetDefinition new addProject: changedProjectDefinition; yourself.
	projectSetModification := readProjectSetDefinition compareAgainstBase: writeProjectSetDefinition.
	self assert: projectSetModification isEmpty.
]

{ #category : 'tests' }
RwProjectFiletreeTonelReaderWriterTest >> testWriterReader_E [

	"https://github.com/GemTalk/Rowan/issues/361"

	"write repository to an alternate root directory and rewrite ... without error"

	| projectName writtenProjectDefinition copyProjectDefinition copyProjectSetDefinition 
		projectSetModification writeProjectSetDefinition oldRepositoryRoot newRepositoryRoot |
	projectName := 'Issue361'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

"write projectDefinition to disk"
	writtenProjectDefinition := self _projectDefinitionForStructureWriters_A: projectName format: self _repositoryFormat.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	writtenProjectDefinition repositoryRoot ensureDeleteAll.
	writtenProjectDefinition resolveProject; export.

"copy project definition and write to an alternate repository root"
	copyProjectDefinition := writtenProjectDefinition copy.
	oldRepositoryRoot := writtenProjectDefinition repositoryRoot.
	newRepositoryRoot := oldRepositoryRoot parent / (copyProjectDefinition name, '_COPY').
	newRepositoryRoot ensureDeleteAll.
	copyProjectDefinition diskRepositoryRoot: newRepositoryRoot pathString.

	copyProjectDefinition export.

"should be able to writeover and over and over again ... once is enough for me:)"
	copyProjectDefinition export.

"read from alternate root directory"
	copyProjectSetDefinition := copyProjectDefinition readProjectSet.

"validation"
	writeProjectSetDefinition := RwProjectSetDefinition new addProject: writtenProjectDefinition; yourself.
	projectSetModification := copyProjectSetDefinition compareAgainstBase: writeProjectSetDefinition.
	self assert: projectSetModification isEmpty.
]
