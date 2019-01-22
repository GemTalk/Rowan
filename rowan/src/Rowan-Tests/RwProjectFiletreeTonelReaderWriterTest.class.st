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

	RwRepositoryComponentProjectReaderVisitor new visit: projectDefinition.
self error: 'not yet implemented'.

]

{ #category : 'private' }
RwProjectFiletreeTonelReaderWriterTest >> _repositoryFormat [

	^ self subclassResponsibility: #_repositoryFormat
]

{ #category : 'private' }
RwProjectFiletreeTonelReaderWriterTest >> _repositoryPropertyDictFor: aProjectDefinition repositoryRootPath: repositoryRootPath [

	| propertiesFile |
	propertiesFile := repositoryRootPath / 'properties' , 'st'.
	propertiesFile exists
		ifFalse: [
			propertiesFile := repositoryRootPath / '.filetree'.
			propertiesFile exists
				ifFalse: [ propertiesFile := repositoryRootPath / '.cypress' ] ].
	^ STON fromStream: (ZnBufferedReadStream on: propertiesFile  readStream)
]

{ #category : 'private' }
RwProjectFiletreeTonelReaderWriterTest >> _writerVisitorClass [

	^ self subclassResponsibility: #_writerVisitorClass
]

{ #category : 'tests' }
RwProjectFiletreeTonelReaderWriterTest >> testReadExistingDiskProject [

	| rowanSpec projectHome specUrlString projectSpec projectDefinition |	
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	projectHome := rowanSpec repositoryRootPath , '/test/testRepositories/'.

	specUrlString :=  'file:' , projectHome, '/Issue122/', self _repositoryFormat, '/rowan/specs/Issue122.ston'.

	projectSpec := specUrlString asRwUrl asSpecification.

"create project definition"
	projectDefinition := RwComponentProjectDefinition newForProjectReference: (RwProjectReferenceDefinition newForSpecification: projectSpec).
	projectDefinition projectHome: projectHome.

	Rowan projectTools read 
		readProjectSetForComponentProjectDefinition: projectDefinition.
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
