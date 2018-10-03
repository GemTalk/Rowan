Class {
	#name : 'RwProjectWriterTest',
	#superclass : 'RwBrowserToolTest',
	#instVars : [
		'filesystem',
		'toDelete'
	],
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwProjectWriterTest >> _createFileSystem [
	^ FileSystem memory
]

{ #category : 'private' }
RwProjectWriterTest >> _markForCleanup: anObject [
	toDelete add: (filesystem resolve: anObject)
]

{ #category : 'running' }
RwProjectWriterTest >> setUp [
	super setUp.
	filesystem := self _createFileSystem.
	toDelete := OrderedCollection new.
]

{ #category : 'running' }
RwProjectWriterTest >> tearDown [
	toDelete
		select: [ :path | filesystem exists: path ]
		thenDo: [ :path | filesystem delete: path ].
	super tearDown
]

{ #category : 'tests' }
RwProjectWriterTest >> testGsTopazWriter [
	|  projectName packageName1 packageName2 projectDefinition classDefinition packageDefinition className
		classExtensionDefinition projectSetDefinition projectSetModification visitor repositoryRootPath |

	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension'.
	className := 'Issue361Class1'. 

"create definitions"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName1;
		setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName2;
		yourself.

	classDefinition := RwClassDefinition
		newForClassNamed: className
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
	packageDefinition := projectDefinition packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: '*', packageName2 asLowercase
					source: 'method1 ^1').
	packageDefinition := projectDefinition packageNamed: packageName2.
	packageDefinition addClassExtension: classExtensionDefinition.

"diff"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.

"write"
	repositoryRootPath := filesystem workingDirectory / projectName / 'topaz'.
	self _markForCleanup: repositoryRootPath.
	repositoryRootPath ensureCreateDirectory.

	projectSetModification := projectSetDefinition compareAgainstBase: RwProjectSetDefinition new.
	visitor := RwGsModificationTopazWriterVisitor new
		repositoryRootPath: repositoryRootPath;
		topazFilename: projectName;
		yourself.
	visitor visit: projectSetModification.
]
