Class {
	#name : 'RwProjectReaderWriterTest',
	#superclass : 'RwBrowserToolTest',
	#instVars : [
		'filesystem',
		'toDelete',
		'projectsToDelete'
	],
	#category : 'Rowan-Tests'
}

{ #category : 'testing' }
RwProjectReaderWriterTest class >> isAbstract [
  "Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

  ^ self sunitName = #'RwProjectReaderWriterTest'
]

{ #category : 'private' }
RwProjectReaderWriterTest >> _createFileSystem [
	^ FileSystem disk "currentMemoryFileSystem"
]

{ #category : 'private' }
RwProjectReaderWriterTest >> _markForCleanup: aFileReference [
	toDelete add: aFileReference
]

{ #category : 'private' }
RwProjectReaderWriterTest >> _markForProjectCleanup: aProjectName [
	projectsToDelete add: aProjectName
]

{ #category : 'private' }
RwProjectReaderWriterTest >> _projectDefinitionForStructureWriters_A: projectName format: repositoryFormat [

	"multiple class extensions from multiple packages for multiple classes"

	| packageName1 packageName2 projectDefinition classDefinition packageDefinition className1 className2 className3
		classExtensionDefinition packageName3 rowanSpec projectHome |

	self _markForProjectCleanup: projectName.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension1'.
	packageName3 := 'Issue361-Extension2'.
	className1 := 'Issue361Class1'. 
	className2 := 'Issue361Class2'. 
	className3 := 'Issue361Class3'. 

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	projectHome := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.


"create definitions"
	projectDefinition := (RwComponentProjectDefinition
		newForDiskBasedProjectNamed: projectName)
		packageFormat: repositoryFormat;
		projectHome: projectHome;
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
		comment: 'this is a test comment for ', className1
		pools: #()
		type: 'normal'.
	classDefinition
		addInstanceMethodDefinition: 
			(RwMethodDefinition
					newForSelector: #'method6'
					protocol: 'instance accessing'
					source: 'method6 ^6');
		addClassMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method2'
					protocol: 'class accessing'
					source: 'method2 ^2').
	packageDefinition addClassDefinition: classDefinition.

	classDefinition := RwClassDefinition
		newForClassNamed: className2
		super: 'Array'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: 'this is a test comment for ', className2
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
		comment: 'this is a test comment for ', className3
		pools: #()
		type: 'normal'.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method8:'
					protocol: 'accessing'
					source: 'method8: ignored ^ignored');
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method9'
					protocol: 'accessing'
					source: 'method9 ^9');
		addClassMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method4'
					protocol: 'accessing'
					source: 'method4 ^4');
		addClassMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method10'
					protocol: 'accessing'
					source: 'method10 ^10').
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
					source: 'method5 ^5');
		addClassMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method6'
					protocol: '*', packageName3 asLowercase
					source: 'method6 ^6').
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	^ projectDefinition
]

{ #category : 'running' }
RwProjectReaderWriterTest >> setUp [
	super setUp.
	filesystem := self _createFileSystem.
	toDelete := OrderedCollection new.
	projectsToDelete := OrderedCollection new.
]

{ #category : 'running' }
RwProjectReaderWriterTest >> tearDown [
	toDelete
		select: [ :aFileRef | aFileRef exists ]
		thenDo: [ :aFileRef | aFileRef deleteAll ].
	projectsToDelete
		do: [ :projectName | 
		(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
			ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].
			(Rowan image projectRepositoryNamed: projectName ifAbsent: [  ])
				ifNotNil: [ :repo | Rowan image _removeProjectRepository: repo ] ].
	super tearDown
]
