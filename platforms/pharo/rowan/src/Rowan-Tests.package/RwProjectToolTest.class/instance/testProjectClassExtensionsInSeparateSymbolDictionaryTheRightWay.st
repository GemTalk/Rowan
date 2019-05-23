tests
testProjectClassExtensionsInSeparateSymbolDictionaryTheRightWay

	"Proper way to add extension methods to a class --- load spec expanded to allow user to specify per package symbol dictionaries ... symbolDictName redefined as defaultSymbolDictName."

	| projectName projectDefinition projectTools classDefinition packageDefinition1 packageDefinition2 packageDefinition3 className1 className2 testClass1 testClass2 testInstance1 testInstance2 classExtensionDefinition dictionariesAndSymbols x y packageNames |
	projectName := 'Simple'.
	packageNames := #('Simple-Core1' 'Simple-Core2' 'Simple-Extensions1').
	projectTools := Rowan projectTools.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		comment:
				'This is a project created in memory first, then written to disk. There are three packages 21 of which creates classes in a different symbol dictionary.';
		packageNames: packageNames;
		defaultSymbolDictName: self _symbolDictionaryName2;
		setSymbolDictName: self _symbolDictionaryName1 forPackageNamed: 'Simple-Core1';
		setSymbolDictName: self _symbolDictionaryName1
			forPackageNamed: 'Simple-Extensions1';
		yourself.

	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName1.
	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName2.

	className1 := 'Simple1'.
	className2 := 'Simple2'.

	classDefinition := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #('ivar1')
		classinstvars: #(#'civar1')
		classvars: #()
		category: nil
		comment: 'I am a Simple class with extensions'
		pools: #()
		type: 'normal'.

	packageDefinition1 := projectDefinition packageNamed: 'Simple-Core1'.
	packageDefinition1 addClassDefinition: classDefinition.

	projectTools load loadProjectDefinition: projectDefinition.

	testClass1 := Rowan globalNamed: className1.
	self assert: testClass1 notNil.
	testClass2 := Rowan globalNamed: className2.
	self assert: testClass2 isNil.
	testInstance1 := testClass1 new.
	self should: [ testInstance1 ivar1 ] raise: MessageNotUnderstood.

	dictionariesAndSymbols := Rowan image symbolList
		dictionariesAndSymbolsOf: testClass1.
	self assert: dictionariesAndSymbols size = 1.
	self
		assert:
			(x := (dictionariesAndSymbols at: 1) at: 1)
				== (y := Rowan globalNamed: self _symbolDictionaryName1).

	classDefinition := RwClassDefinition
		newForClassNamed: className2
		super: 'Object'
		instvars: #('ivar1')
		classinstvars: #(#'civar1')
		classvars: #()
		category: nil
		comment: 'I am a Simple class with extensions'
		pools: #()
		type: 'normal'.

	packageDefinition2 := projectDefinition packageNamed: 'Simple-Core2'.
	packageDefinition2 addClassDefinition: classDefinition.

	projectTools load loadProjectDefinition: projectDefinition.

	testClass2 := Rowan globalNamed: className2.
	self assert: testClass1 notNil.
	testInstance2 := testClass2 new.

	dictionariesAndSymbols := Rowan image symbolList
		dictionariesAndSymbolsOf: testClass2.
	self assert: dictionariesAndSymbols size = 1.
	self
		assert:
			(x := (dictionariesAndSymbols at: 1) at: 1)
				== (y := Rowan globalNamed: self _symbolDictionaryName2).

	classExtensionDefinition := RwClassExtensionDefinition
		newForClassNamed: className1.
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

	packageDefinition3 := projectDefinition packageNamed: 'Simple-Extensions1'.
	packageDefinition3 addClassExtensionDefinition: classExtensionDefinition.

	[ projectTools load loadProjectDefinition: projectDefinition ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | ex resume: true ].

	self assert: testClass1 civar1 = 1.

	self
		handleConfirmationDuring: [ 
			projectTools create
				createProjectFor: projectDefinition
				format: 'tonel'
				root: '/tmp/rowanClassExtensionsProject/'
				configsPath: 'configs'
				repoPath: 'src' 
				specsPath: 'specs' ].

	projectTools spec exportProjectDefinition: projectDefinition.
	projectTools write writeProjectDefinition: projectDefinition.
	projectTools commit
		commitProjectDefinition: projectDefinition
		message:
			'3 packages with extension methods for first package in third package.'