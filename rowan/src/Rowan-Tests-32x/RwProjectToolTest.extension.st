Extension { #name : 'RwProjectToolTest' }

{ #category : '*rowan-tests-32x' }
RwProjectToolTest >> testDiskSimpleProject1 [

	"Create project and build disk-based artifacts first, then create create a class and write changes to disk."

	| projectName projectDefinition projectTools classDefinition packageDefinition packageNames loadedProject |
	projectName := 'Simple'.
	packageNames := #('Simple-Core' 'Simple-Tests').
	projectTools := Rowan projectTools.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	self
		handleConfirmationDuring: [ 
			projectDefinition := projectTools create
				createDiskBasedProject: projectName
				packageNames: packageNames
				format: 'tonel'
				root: self _testRowanProjectsSandbox pathString, '/rowanSimpleProject/'].
	projectDefinition
		comment:
				'This is a simple project to demonstrate the smalltalk API used for a project lifecycle';
		yourself.
	projectDefinition defaultSymbolDictName: self _symbolDictionaryName1.
	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName1.

	projectTools spec exportProjectDefinition: projectDefinition.
	projectTools write writeProjectDefinition: projectDefinition.
	projectDefinition commit: 'Initial commit'.
	self
		handleInformAsFailureDuring: [ projectTools load loadProjectDefinition: projectDefinition ].

	loadedProject := Rowan image
		loadedProjectNamed: projectName
		ifAbsent: [ self assert: false description: 'expected to find loaded project' ].
	packageNames
		do: [ :packageName | 
			"ensure that we have a loaded package for each of the packages"
			Rowan image
				loadedPackageNamed: packageName
				ifAbsent: [ self assert: false description: 'expected to find loaded package' ] ].

	classDefinition := RwClassDefinition
		newForClassNamed: 'Simple'
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: nil
		comment: 'I am a Simple class'
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition packageNamed: 'Simple-Core'.
	packageDefinition addClassDefinition: classDefinition.

	self
		handleInformAsFailureDuring: [ projectTools load loadProjectDefinition: projectDefinition ].

	projectTools spec exportSpecification: projectDefinition specification.
	projectTools write writeProjectDefinition: projectDefinition.
	projectDefinition commit: 'Added Simple class'
]

{ #category : '*rowan-tests-32x' }
RwProjectToolTest >> testProjectClassExtensions [

	"Build our project in memory without committing to disk until we've created a class with methods, then write to disk."

	| projectName projectDefinition projectTools classDefinition packageDefinition1 packageDefinition2 className testClass testInstance classExtensionDefinition packageNames |
	projectName := 'Simple'.
	packageNames := #('Simple-Core' 'Simple-Extensions' 'Simple-Tests').
	projectTools := Rowan projectTools.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	(self _testRowanProjectsSandbox pathString, '/rowanSimpleExtensionProject/') asFileReference ensureDeleteAll.

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
				root: self _testRowanProjectsSandbox pathString, '/rowanSimpleExtensionProject/'
				configsPath: 'configs'
				repoPath: 'src' 
				specsPath: 'specs' ].

	projectTools spec exportProjectDefinition: projectDefinition.
	projectTools write writeProjectDefinition: projectDefinition.
	projectDefinition commit: 'Added Simple class and extension methods'
]

{ #category : '*rowan-tests-32x' }
RwProjectToolTest >> testProjectClassExtensionsInSeparateSymbolDictionaryTheRightWay [

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
				root: self _testRowanProjectsSandbox pathString, '/rowanClassExtensionsProject/'
				configsPath: 'configs'
				repoPath: 'src' 
				specsPath: 'specs' ].

	projectTools spec exportProjectDefinition: projectDefinition.
	projectTools write writeProjectDefinition: projectDefinition.
	projectDefinition commit:
			'3 packages with extension methods for first package in third package.'
]

{ #category : '*rowan-tests-32x' }
RwProjectToolTest >> testSimpleProject1 [

	"Create project and build disk-based artifacts first, then create create a class and write changes to disk."

	| projectName projectDefinition projectTools classDefinition packageDefinition packageNames loadedProject |
	projectName := 'Simple'.
	packageNames := #('Simple-Core' 'Simple-Tests').
	projectTools := Rowan projectTools.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	(self _testRowanProjectsSandbox pathString, '/rowanSimpleProject/') asFileReference ensureDeleteAll.

	self
		handleConfirmationDuring: [ 
			projectDefinition := projectTools create
				createGitBasedProject: projectName
				packageNames: packageNames
				format: 'tonel'
				root: self _testRowanProjectsSandbox pathString, '/rowanSimpleProject/' ].
	projectDefinition
		comment:
				'This is a simple project to demonstrate the smalltalk API used for a project lifecycle';
		yourself.
	projectDefinition defaultSymbolDictName: self _symbolDictionaryName1.
	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName1.

	projectTools spec exportProjectDefinition: projectDefinition.
	projectTools write writeProjectDefinition: projectDefinition.
	projectDefinition commit: 'Initial commit'.
	self
		handleInformAsFailureDuring: [ projectTools load loadProjectDefinition: projectDefinition ].

	loadedProject := Rowan image
		loadedProjectNamed: projectName
		ifAbsent: [ self assert: false description: 'expected to find loaded project' ].
	packageNames
		do: [ :packageName | 
			"ensure that we have a loaded package for each of the packages"
			Rowan image
				loadedPackageNamed: packageName
				ifAbsent: [ self assert: false description: 'expected to find loaded package' ] ].

	classDefinition := RwClassDefinition
		newForClassNamed: 'Simple'
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: nil
		comment: 'I am a Simple class'
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition packageNamed: 'Simple-Core'.
	packageDefinition addClassDefinition: classDefinition.

	self
		handleInformAsFailureDuring: [ projectTools load loadProjectDefinition: projectDefinition ].

	projectTools spec exportSpecification: projectDefinition specification.
	projectTools write writeProjectDefinition: projectDefinition.
	projectDefinition commit: 'Added Simple class'
]

{ #category : '*rowan-tests-32x' }
RwProjectToolTest >> testSimpleProject2 [

	"Build our project in memory without committing to disk until we've created a class, then write to disk."

	| projectName projectDefinition projectTools classDefinition packageDefinition packageNames |
	projectName := 'Simple'.
	packageNames := #('Simple-Core' 'Simple-Tests').
	projectTools := Rowan projectTools.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	(self _testRowanProjectsSandbox pathString, '/rowanSimpleProject2/') asFileReference ensureDeleteAll.

	projectDefinition := RwProjectDefinition
		newForGitBasedProjectNamed: projectName.
	projectDefinition
		comment:
				'This is a simple project created in memory first, then written to disk.';
		packageNames: packageNames;
		yourself.
	projectDefinition defaultSymbolDictName: self _symbolDictionaryName1.

	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName1.

	classDefinition := RwClassDefinition
		newForClassNamed: 'Simple2'
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: nil
		comment: 'I am a Simple2 class'
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition packageNamed: 'Simple-Core'.
	packageDefinition addClassDefinition: classDefinition.

	projectTools load loadProjectDefinition: projectDefinition.

	self
		handleConfirmationDuring: [ 
			projectTools create
				createProjectFor: projectDefinition
				format: 'tonel'
				root: self _testRowanProjectsSandbox pathString, '/rowanSimpleProject2/'
				configsPath: 'configs'
				repoPath: 'src' 
				specsPath: 'specs' ].

	projectTools spec exportProjectDefinition: projectDefinition.
	projectTools write writeProjectDefinition: projectDefinition.
	projectDefinition commit: 'Added Simple class'
]

{ #category : '*rowan-tests-32x' }
RwProjectToolTest >> testSimpleProject3 [

	"Build our project in memory without committing to disk until we've created a class with methods, then write to disk."

	| projectName projectDefinition projectTools classDefinition packageDefinition className testClass testInstance packageNames |
	projectName := 'Simple'.
	packageNames := #('Simple-Core' 'Simple-Tests').
	projectTools := Rowan projectTools.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	(self _testRowanProjectsSandbox pathString, '/rowanSimpleProject3/') asFileReference ensureDeleteAll.

	projectDefinition := RwProjectDefinition
		newForGitBasedProjectNamed: projectName.
	projectDefinition
		comment:
				'This is a simple project created in memory first, then written to disk.';
		packageNames: packageNames;
		yourself.
	projectDefinition defaultSymbolDictName: self _symbolDictionaryName1.

	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName1.

	className := 'Simple3'.
	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #('ivar1')
		classinstvars: #(#'civar1')
		classvars: #()
		category: nil
		comment: 'I am a Simple3 class'
		pools: #()
		type: 'normal'.

	classDefinition
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

	packageDefinition := projectDefinition packageNamed: 'Simple-Core'.
	packageDefinition addClassDefinition: classDefinition.

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
				root: self _testRowanProjectsSandbox pathString, '/rowanSimpleProject3/'
				configsPath: 'configs'
				repoPath: 'src' 
				specsPath: 'specs' ].

	projectTools spec exportProjectDefinition: projectDefinition.
	projectTools write writeProjectDefinition: projectDefinition.
	projectDefinition commit: 'Added Simple3 class and methods'
]
