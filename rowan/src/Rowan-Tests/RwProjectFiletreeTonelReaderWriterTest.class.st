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
RwProjectFiletreeTonelReaderWriterTest >> _projectSetDefinitionForStructureWriters_A [

	"multiple class extensions from multiple packages for multiple classes"

	|  projectName packageName1 packageName2 projectDefinition classDefinition packageDefinition className1 className2 className3
		classExtensionDefinition projectSetDefinition packageName3 |

	projectName := 'Issue361'.
	self _markForProjectCleanup: projectName.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension1'.
	packageName3 := 'Issue361-Extension2'.
	className1 := 'Issue361Class1'. 
	className2 := 'Issue361Class2'. 
	className3 := 'Issue361Class3'. 

"create definitions"
	projectDefinition := (RwProjectDefinition
		newForDiskBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		addPackageNamed: packageName3;
		setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName1;
		setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName2;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName1.

	classDefinition := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition
		addClassMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method2'
					protocol: 'accessing'
					source: 'method2 ^2').
	packageDefinition addClassDefinition: classDefinition.

	classDefinition := RwClassDefinition
		newForClassNamed: className2
		super: 'Array'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method3'
					protocol: 'accessing'
					source: 'method3 ^3').
	packageDefinition addClassDefinition: classDefinition.

	classDefinition := RwClassDefinition
		newForClassNamed: className3
		super: className1
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition
		addClassMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method4'
					protocol: 'accessing'
					source: 'method4 ^4').
	packageDefinition addClassDefinition: classDefinition.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: '*', packageName2 asLowercase
					source: 'method1 ^1').
	packageDefinition := projectDefinition packageNamed: packageName2.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	packageDefinition := projectDefinition packageNamed: packageName3.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method5'
					protocol: '*', packageName3 asLowercase
					source: 'method5 ^5').
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

"project set"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.

	^ projectSetDefinition
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

	| rowanSpec projectHome specUrlString projectDefinition |	
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	projectHome := rowanSpec repositoryRootPath , '/test/testRepositories/'.

"identify spec to be used for reading project"
	specUrlString :=  'file:' , projectHome, '/Issue122/', self _repositoryFormat, '/rowan/specs/Issue122.ston'.

"create project definition"
	projectDefinition := RwComponentProjectDefinition newForUrl: specUrlString.
"point to directory where the disk project is located"
	projectDefinition projectHome: projectHome.

"read project"
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
