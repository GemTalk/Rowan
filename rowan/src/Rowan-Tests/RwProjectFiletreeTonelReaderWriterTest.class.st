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
RwProjectFiletreeTonelReaderWriterTest >> _classExtensionRemovedArtifactFileReference: repositoryRoot [

	self subclassResponsibility: #_classExtensionRemovedArtifactFileReference:
]

{ #category : 'private' }
RwProjectFiletreeTonelReaderWriterTest >> _classRemovedArtifactFileReference: repositoryRoot [

	self subclassResponsibility: #_classRemovedArtifactFileReference:
]

{ #category : 'private' }
RwProjectFiletreeTonelReaderWriterTest >> _packageRemovedArtifactFileReference: repositoryRoot [

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
RwProjectFiletreeTonelReaderWriterTest >> testReadExistingDiskProject [

	| rowanSpec projectHome specUrlString projectDefinition projectDefinitionSet |	
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	projectHome := rowanSpec repositoryRootPath , '/test/testRepositories/'.

"identify spec to be used for reading project"
	specUrlString :=  'file:' , projectHome, '/Issue122/', self _repositoryFormat, '/rowan/specs/Issue122.ston'.

"create project definition"
	projectDefinition := RwComponentProjectDefinition newForUrl: specUrlString.
"point to directory where the disk project is located"
	projectDefinition projectHome: projectHome.

"read project"
	projectDefinitionSet := Rowan projectTools read 
		readProjectSetForComponentProjectDefinition: projectDefinition.

"validation"
	self _validateIssue122ProjectDefinitionSet: projectDefinitionSet projectName: projectDefinition name
]

{ #category : 'tests' }
RwProjectFiletreeTonelReaderWriterTest >> testReadExistingDiskProjectWithBothMethodProtocolValidationError [
	"https://github.com/GemTalk/Rowan/issues/122"

	"method protocol starts with a * for non-extension method"

	| rowanSpec projectHome specUrlString projectDefinition projectDefinitionSet patches |	
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	projectHome := rowanSpec repositoryRootPath , '/test/testRepositories/'.

"identify spec to be used for reading project"
	specUrlString :=  'file:' , projectHome, '/Issue122/', self _repositoryFormat, '/rowan/specs/Issue122_Both-ProtocolValidationError.ston'.

"create project definition"
	projectDefinition := RwComponentProjectDefinition newForUrl: specUrlString.
"point to directory where the disk project is located"
	projectDefinition projectHome: projectHome.

"read project -- catch and resume the notification ... repair the protocol"
	[ projectDefinitionSet := Rowan projectTools read readProjectSetForComponentProjectDefinition: projectDefinition ]
		on: RwInvalidCategoryProtocolConventionErrorNotification
		do: [:ex | 
			ex
				classCategoryNotificationDo: [ self assert: 'Unexpected notification ', ex class printString ]
				nonExtensionMethodNoficationDo: [ 
					"repair the protocol"
					ex methodDefinition protocol: 'repaired'.
					ex resume ] 
				extensionMethodPackageMismatchNoficationDo: [ 
					"repair the protocol - extension method protocol does start with a *, but does not match the package name"
					ex methodDefinition protocol: '*', ex packageName asLowercase.
					ex resume ]  
				extensionMethodNonExtensionProtocolNoficationDo: [ 
					"repair the protocol - extension method protocol doesn't start with a *"
					ex methodDefinition protocol: '*', ex packageName asLowercase.
					ex resume ] ].

"validation - read from disk, without repairing the issues (creating a patch reads without repairing validation errors"
	patches := Rowan projectTools diff patchesForProjectDefinition: projectDefinition.
	self assert: patches size = 1.
	patches do: [:assoc |
		| packageName patch operations |
		packageName := assoc key.
		patch := assoc value.
		self assert: packageName = 'Issue122-Extension1-ProtocolValidationError'.
		operations := patch operations.
		self assert: operations size = 2.
		operations do: [:modification |
			| definition |
			definition := modification modification.
			definition selector = 'method1'
				ifTrue: [ self assert: definition category = '*issue122-extension1-protocolvalidationerror' ]
				ifFalse: [
					definition selector = 'method20'
						ifTrue: [ self assert: definition category = '*issue122-extension1-protocolvalidationerror' ]
						ifFalse: [ self assert: false description: 'unknown definition ', definition printString ] ] ] ]
]

{ #category : 'tests' }
RwProjectFiletreeTonelReaderWriterTest >> testReadExistingDiskProjectWithClassCategoryValidationError [
	"https://github.com/GemTalk/Rowan/issues/122"

	"class category does not match package name -- Rowan Hybrid"

	| rowanSpec projectHome specUrlString projectDefinition projectDefinitionSet |	
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	projectHome := rowanSpec repositoryRootPath , '/test/testRepositories/'.

"identify spec to be used for reading project"
	specUrlString :=  'file:' , projectHome, '/Issue122/', self _repositoryFormat, '/rowan/specs/Issue122_CategoryValidationError.ston'.

"create project definition"
	projectDefinition := RwComponentProjectDefinition newForUrl: specUrlString.
"point to directory where the disk project is located"
	projectDefinition projectHome: projectHome.

"read project -- hit category does not match error"
	self 
		should: [ Rowan projectTools read readProjectSetForComponentProjectDefinition: projectDefinition ]
		raise: Error.

"recreate project definition"
	projectDefinition := RwComponentProjectDefinition newForUrl: specUrlString.

"point to directory where the disk project is located"
	projectDefinition projectHome: projectHome.

"read project -- catch and resume the notification ... repair the category"
	[ projectDefinitionSet := Rowan projectTools read readProjectSetForComponentProjectDefinition: projectDefinition ]
		on: RwInvalidCategoryProtocolConventionErrorNotification
		do: [:ex | 
			ex
				classCategoryNotificationDo: [
					"repair the class category"
					ex classDefinition category: ex packageName.
					ex resume: true ]
				nonExtensionMethodNoficationDo: [ self assert: 'Unexpected notification ', ex class printString ] 
				extensionMethodPackageMismatchNoficationDo: [ self assert: 'Unexpected notification ', ex class printString ]  
				extensionMethodNonExtensionProtocolNoficationDo: [ self assert: 'Unexpected notification ', ex class printString ] ].

"validation"
	self _validateIssue122Repaired_CategoryValidationError_ProjectDefinitionSet: projectDefinitionSet projectName: projectDefinition name
]

{ #category : 'tests' }
RwProjectFiletreeTonelReaderWriterTest >> testReadExistingDiskProjectWithEmptyClassExtension [
	"https://github.com/GemTalk/Rowan/issues/361"


	| rowanSpec projectHome projectName specUrlString projectDefinition projectDefinitionSet |	
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	projectHome := rowanSpec repositoryRootPath , '/test/testRepositories/'.

"identify spec to be used for reading project"
	projectName := 'Issue361'.
	specUrlString :=  'file:' , projectHome, '/', projectName, '/', self _repositoryFormat, '/rowan/specs/Issue122.ston'.

"create project definition"
	projectDefinition := RwComponentProjectDefinition newForUrl: specUrlString.
"point to directory where the disk project is located"
	projectDefinition projectHome: projectHome.

"read project -- hit protocol is invalid error"
	projectDefinitionSet := Rowan projectTools read readProjectSetForComponentProjectDefinition: projectDefinition.

"validate"
	self _validateIssue361ProjectDefinitionSet: projectDefinitionSet projectName: projectName
]

{ #category : 'tests' }
RwProjectFiletreeTonelReaderWriterTest >> testReadExistingDiskProjectWithExtensionMethodProtocolValidationError [
	"https://github.com/GemTalk/Rowan/issues/122"

	"
		extension method protocol doesn't start with a *
			OR
		extension method protocol does start with a *, but does not match the package name
	"

	| rowanSpec projectHome specUrlString projectDefinition projectDefinitionSet |	
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	projectHome := rowanSpec repositoryRootPath , '/test/testRepositories/'.

"identify spec to be used for reading project"
	specUrlString :=  'file:' , projectHome, '/Issue122/', self _repositoryFormat, '/rowan/specs/Issue122_Extension-ProtocolValidationError.ston'.

"create project definition"
	projectDefinition := RwComponentProjectDefinition newForUrl: specUrlString.
"point to directory where the disk project is located"
	projectDefinition projectHome: projectHome.

"read project -- hit protocol is invalid error"
	self 
		should: [ Rowan projectTools read readProjectSetForComponentProjectDefinition: projectDefinition ]
		raise: Error.

"recreate project definition"
	projectDefinition := RwComponentProjectDefinition newForUrl: specUrlString.

"point to directory where the disk project is located"
	projectDefinition projectHome: projectHome.

"read project -- catch and resume the notification ... repair the protocol"
	[ projectDefinitionSet := Rowan projectTools read readProjectSetForComponentProjectDefinition: projectDefinition ]
		on: RwInvalidCategoryProtocolConventionErrorNotification
		do: [:ex | 
			ex
				classCategoryNotificationDo: [ self assert: 'Unexpected notification ', ex class printString ]
				nonExtensionMethodNoficationDo: [ self assert: 'Unexpected notification ', ex class printString ] 
				extensionMethodPackageMismatchNoficationDo: [ 
					"repair the protocol - extension method protocol does start with a *, but does not match the package name"
					ex methodDefinition protocol: '*', ex packageName asLowercase.
					ex resume ]  
				extensionMethodNonExtensionProtocolNoficationDo: [ 
					"repair the protocol - extension method protocol doesn't start with a *"
					ex methodDefinition protocol: '*', ex packageName asLowercase.
					ex resume ] ].

"validation"
	self _validateIssue122Repaired_ExtensionProtocolValidationError_ProjectDefinitionSet: projectDefinitionSet projectName: projectDefinition name
]

{ #category : 'tests' }
RwProjectFiletreeTonelReaderWriterTest >> testReadExistingDiskProjectWithMethodProtocolValidationError [
	"https://github.com/GemTalk/Rowan/issues/122"

	"method protocol starts with a * for non-extension method"

	| rowanSpec projectHome specUrlString projectDefinition projectDefinitionSet |	
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	projectHome := rowanSpec repositoryRootPath , '/test/testRepositories/'.

"identify spec to be used for reading project"
	specUrlString :=  'file:' , projectHome, '/Issue122/', self _repositoryFormat, '/rowan/specs/Issue122_Core-ProtocolValidationError.ston'.

"create project definition"
	projectDefinition := RwComponentProjectDefinition newForUrl: specUrlString.
"point to directory where the disk project is located"
	projectDefinition projectHome: projectHome.

"read project -- hit protocol is invalid error"
	self 
		should: [ Rowan projectTools read readProjectSetForComponentProjectDefinition: projectDefinition ]
		raise: Error.

"recreate project definition"
	projectDefinition := RwComponentProjectDefinition newForUrl: specUrlString.

"point to directory where the disk project is located"
	projectDefinition projectHome: projectHome.

"read project -- catch and resume the notification ... repair the protocol"
	[ projectDefinitionSet := Rowan projectTools read readProjectSetForComponentProjectDefinition: projectDefinition ]
		on: RwInvalidCategoryProtocolConventionErrorNotification
		do: [:ex | 
			ex
				classCategoryNotificationDo: [ self assert: 'Unexpected notification ', ex class printString ]
				nonExtensionMethodNoficationDo: [ 
					"repair the protocol"
					ex methodDefinition protocol: 'repaired'.
					ex resume ] 
				extensionMethodPackageMismatchNoficationDo: [ self assert: 'Unexpected notification ', ex class printString ]  
				extensionMethodNonExtensionProtocolNoficationDo: [ self assert: 'Unexpected notification ', ex class printString ] ].

"validation"
	self _validateIssue122Repaired_CoreProtocolValidationError_ProjectDefinitionSet: projectDefinitionSet projectName: projectDefinition name
]

{ #category : 'tests' }
RwProjectFiletreeTonelReaderWriterTest >> testWriterReader_A [

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
]

{ #category : 'tests' }
RwProjectFiletreeTonelReaderWriterTest >> testWriterReader_B_removeClass [

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
]

{ #category : 'tests' }
RwProjectFiletreeTonelReaderWriterTest >> testWriterReader_B_removeExtensionClass [

	"https://github.com/GemTalk/Rowan/issues/361"

	"Set of tests that add, change, and remove classes, methods, and extension methods; write to an existing disk repo.
		Expecting to incrementally write only the changed definitions"

	| projectName writtenProjectDefinition readProjectSetDefinition changedProjectSetDefinition visitor
		projectSetModification writeProjectSetDefinition changedProjectDefinition changedProjectSetModification
		writerVisitorClass writtenPojectSetDefinition repositoryRoot x |

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
	self assert: (x := self _classExtensionRemovedArtifactFileReference: repositoryRoot) exists.

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

	self deny: (self _classExtensionRemovedArtifactFileReference: repositoryRoot) exists
]

{ #category : 'tests' }
RwProjectFiletreeTonelReaderWriterTest >> testWriterReader_B_removePackage [

	"https://github.com/GemTalk/Rowan/issues/361"

	"Set of tests that add, change, and remove classes, methods, and extension methods; write to an existing disk repo.
		Expecting to incrementally write only the changed definitions"

	| projectName writtenProjectDefinition readProjectSetDefinition changedProjectSetDefinition visitor
		projectSetModification writeProjectSetDefinition changedProjectDefinition 
		changedProjectSetModification writerVisitorClass writtenPojectSetDefinition x repositoryRoot |

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
	self assert: (x := self _packageRemovedArtifactFileReference: repositoryRoot) exists.

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
	readProjectSetDefinition := writtenProjectDefinition readProjectSet.
	writeProjectSetDefinition := RwProjectSetDefinition new addProject: changedProjectDefinition; yourself.
	projectSetModification := readProjectSetDefinition compareAgainstBase: writeProjectSetDefinition.
	self assert: projectSetModification isEmpty.

	self deny: (self _packageRemovedArtifactFileReference: repositoryRoot) exists.
]
