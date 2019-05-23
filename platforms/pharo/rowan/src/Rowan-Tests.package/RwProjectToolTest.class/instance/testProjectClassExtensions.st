tests
testProjectClassExtensions

	"Build our project in memory without committing to disk until we've created a class with methods, then write to disk."

	| projectName projectDefinition projectTools classDefinition packageDefinition1 packageDefinition2 className testClass testInstance classExtensionDefinition packageNames |
	projectName := 'Simple'.
	packageNames := #('Simple-Core' 'Simple-Extensions' 'Simple-Tests').
	projectTools := Rowan projectTools.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		comment:
				'This is a simple project created in memory first, then written to disk.';
		packageNames: packageNames;
		yourself.
	projectDefinition defaultSymbolDictName: self _symbolDictionaryName1.

	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName1.

	className := 'Simple'.
	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #('ivar1')
		classinstvars: #(#'civar1')
		classvars: #()
		category: nil
		comment: 'I am a Simple class with extensions'
		pools: #()
		type: 'normal'.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'foo'
				protocol: 'accessing'
				source: 'foo ^ true').

	packageDefinition1 := projectDefinition packageNamed: 'Simple-Core'.
	packageDefinition1 addClassDefinition: classDefinition.

	projectTools load loadProjectDefinition: projectDefinition.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	testInstance := testClass new.
	self assert: testInstance foo.

	classExtensionDefinition := RwClassExtensionDefinition
		newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'ivar1'
						protocol: 'accessing'
						source: 'ivar1 ^ivar1');
		addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'initialize'
						protocol: 'initialization'
						source: 'initialize civar1 := 1.');
		addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'civar1'
						protocol: 'accessing'
						source: 'civar1 ^civar1');
		yourself.

	packageDefinition2 := projectDefinition packageNamed: 'Simple-Extensions'.
	packageDefinition2 addClassExtensionDefinition: classExtensionDefinition.

	[ projectTools load loadProjectDefinition: projectDefinition ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | ex resume: true ].

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	self assert: testClass civar1 == 1.
	testInstance := testClass new.
	self assert: testInstance ivar1 isNil.

	self
		handleConfirmationDuring: [ 
			projectTools create
				createProjectFor: projectDefinition
				format: 'tonel'
				root: '/tmp/rowanSimpleExtensionProject/'
				configsPath: 'configs'
				repoPath: 'src' 
				specsPath: 'specs' ].

	projectTools spec exportProjectDefinition: projectDefinition.
	projectTools write writeProjectDefinition: projectDefinition.
	projectTools commit
		commitProjectDefinition: projectDefinition
		message: 'Added Simple class and extension methods'