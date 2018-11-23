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
RwProjectReaderWriterTest >> _projectSetDefinitionForStructureWriters_A [

	"multiple class extensions from multiple packages for multiple classes"

	|  projectName packageName1 packageName2 projectDefinition classDefinition packageDefinition className1 className2 className3
		classExtensionDefinition projectSetDefinition projectSetModification visitor repositoryRootPath packageName3 |

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
	packageDefinition addClassExtension: classExtensionDefinition.

	packageDefinition := projectDefinition packageNamed: packageName3.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method5'
					protocol: '*', packageName3 asLowercase
					source: 'method5 ^5').
	packageDefinition addClassExtension: classExtensionDefinition.

"project set"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.

	^ projectSetDefinition
]

{ #category : 'private' }
RwProjectReaderWriterTest >> _writeProjectSetDefinition: projectSetDefinition writerVisitorClass: writerVisitorClass format: repositoryFormat [

	| projectName projectDefinition projectSetModification visitor repositoryRootPath |
"project set"
	projectName := projectSetDefinition projectNames at: 1.
	projectDefinition := projectSetDefinition projectNamed: projectName ifAbsent: [].

"set project up for using filesystem"
	repositoryRootPath := filesystem workingDirectory / projectName.
	self _markForCleanup: (filesystem workingDirectory / projectName).

	projectDefinition repositoryRootPath: repositoryRootPath.
	Rowan projectTools create createProjectFor: projectDefinition format:repositoryFormat.

"write project set"
	projectSetModification := projectSetDefinition compareAgainstBase: RwProjectSetDefinition new. "compare against empty project to write out entire project"
	visitor := writerVisitorClass new
		repositoryRootPath: repositoryRootPath;
		yourself.

	visitor visit: projectSetModification.
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
		do: [ :projectName | Rowan projectTools delete deleteProjectNamed: projectName ].
	super tearDown
]
