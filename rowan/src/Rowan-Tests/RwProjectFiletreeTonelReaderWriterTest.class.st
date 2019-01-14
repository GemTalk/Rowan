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
RwProjectFiletreeTonelReaderWriterTest >> _readProjectDefinition: projectDefinition [

	| repoDir packageNames propertyDict format |
	repoDir := filesystem referenceTo: projectDefinition repositoryUrl asRwUrl pathForDirectory.
	propertyDict := self _repositoryPropertyDictFor: projectDefinition repositoryRootPath: repoDir.
	format := propertyDict at: 'format' ifAbsent: [ 'filetree' ].
self error: 'switch readers at this point .. time to rename the Modification visitors or something so that the structure logic can be shared ...'.
	packageNames := repoDir packageNames.
	^ self 
		_readProjectDefinition: projectDefinition 
		packageNames: packageNames
		from: repoDir
]

{ #category : 'private' }
RwProjectFiletreeTonelReaderWriterTest >> _readProjectSetDefinition: projectSetDefinition [

	projectSetDefinition do: [:projectDefinition |
		self _readProjectDefinition: projectDefinition ].
]

{ #category : 'private' }
RwProjectFiletreeTonelReaderWriterTest >> _repositoryFormat [

	^ self subclassResponsibility: #_repositoryFormat
]

{ #category : 'private' }
RwProjectFiletreeTonelReaderWriterTest >> _repositoryPropertyDictFor: aProjectDefinition repositoryRootPath: repositoryRootPath [

	| propertiesFile |
	propertiesFile := repositoryRootPath / '.filetree'.
	propertiesFile exists
		ifFalse: [ propertiesFile := repositoryRootPath / '.cypress' ].
	^ STON fromStream: (ZnBufferedReadStream on: propertiesFile  readStream)
]

{ #category : 'private' }
RwProjectFiletreeTonelReaderWriterTest >> _writerVisitorClass [

	^ self subclassResponsibility: #_writerVisitorClass
]

{ #category : 'tests' }
RwProjectFiletreeTonelReaderWriterTest >> testWriterReader_A [

	"multiple class extensions from multiple packages for multiple classes"

	| projectName writtenProjectDefinition  readProjectDefinition projectModification |
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

	readProjectDefinition := self _readProjectSetDefinition: writtenProjectDefinition.

"validation"
	projectModification := readProjectDefinition compareAgainstBase: writtenProjectDefinition.
	self assert: projectModification isEmpty.
]
