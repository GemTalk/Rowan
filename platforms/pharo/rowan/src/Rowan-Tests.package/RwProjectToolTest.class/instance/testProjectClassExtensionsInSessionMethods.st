tests
testProjectClassExtensionsInSessionMethods

	"This test attempts to add extension methods to a class that is not in the dictionary that the package is being loaded into ... and the extension methods are kept in session methods in the package's symbol dictionary ..."

	| projectName projectDefinition projectTools classDefinition1 classDefinition2 packageDefinition1 packageDefinition2 className1 className2 testClass1 testInstance1 testClass2 testInstance2 classExtensionDefinition packageNames |
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
		defaultSymbolDictName: self _symbolDictionaryName1;
		setSymbolDictName: self _symbolDictionaryName2
			forPackageNamed: 'Simple-Extensions';
		defaultUseSessionMethodsForExtensions: false;
		setUseSessionMethodsForExtensions: true forPackageNamed: 'Simple-Extensions';
		yourself.

	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName1.
	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName2.

	className1 := 'Simple'.
	className2 := 'SimpleAssociation'.

	classDefinition1 := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #('ivar1')
		classinstvars: #(#'civar1')
		classvars: #()
		category: nil
		comment: 'I am a Simple class with extensions'
		pools: #()
		type: 'normal'.

	packageDefinition1 := projectDefinition packageNamed: 'Simple-Core'.
	packageDefinition1 addClassDefinition: classDefinition1.

	projectTools load loadProjectDefinition: projectDefinition.

	testClass1 := Rowan globalNamed: className1.
	self assert: testClass1 notNil.
	testInstance1 := testClass1 new.
	testClass2 := Rowan globalNamed: className2.
	self assert: testClass2 isNil.

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
	classDefinition2 := RwClassDefinition
		newForClassNamed: className2
		super: 'Association'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: nil
		comment: 'I am a SimpleAssociation class'
		pools: #()
		type: 'normal'.
	classDefinition2
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'foo'
				protocol: 'accessing'
				source: 'foo ^ true').

	packageDefinition2 := projectDefinition packageNamed: 'Simple-Extensions'.
	packageDefinition2 addClassDefinition: classDefinition2.
	packageDefinition2 addClassExtensionDefinition: classExtensionDefinition.

	[ projectTools load loadProjectDefinition: projectDefinition ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | ex resume: true ].

	self assert: testClass1 civar1 == 1.
	self assert: testInstance1 ivar1 isNil.
	testClass2 := Rowan globalNamed: className2.
	self assert: testClass2 notNil.
	testInstance2 := testClass2 new.
	self assert: testInstance2 foo