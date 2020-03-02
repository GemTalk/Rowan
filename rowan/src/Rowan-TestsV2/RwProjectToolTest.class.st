Class {
	#name : 'RwProjectToolTest',
	#superclass : 'RwToolTest',
	#category : 'Rowan-TestsV2'
}

{ #category : 'tests' }
RwProjectToolTest >> testProjectClassExtensionsInSeparateSymbolDictionary [

	"This test attempts to add extension methods to a class that is not in the dictionary that the package is being loaded into ... this should actually error out ... all definitions in a package should be applied to a single symbol dictionary ... create separate packages to do cross symbol dictionary updateds ... or possibly use session methods (yet to be determined."

	| projectName1 projectName2 projectDefinition1 projectDefinition2 projectTools classDefinition packageDefinition1 packageNames1 packageDefinition2 packageNames2 className classExtensionDefinition |
	projectName1 := 'Simple'.
	packageNames1 := #('Simple-Core' 'Simple-Tests').
	projectName2 := 'Simple Extensions'.
	packageNames2 := #('Simple-Extensions').
	projectTools := Rowan projectTools.

	{projectName1.
	projectName2}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	projectDefinition1 := ((Rowan globalNamed: 'RwProjectDefinition')
		newForGitBasedProjectNamed: projectName1)
		comment:
				'This is a simple project created in memory first, then written to disk.';
		packageNames: #('Simple-Core' 'Simple-Tests');
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

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

	packageDefinition1 := projectDefinition1 packageNamed: 'Simple-Core'.
	packageDefinition1 addClassDefinition: classDefinition.

	projectTools load loadProjectDefinition: projectDefinition1.

	projectDefinition2 := ((Rowan globalNamed: 'RwProjectDefinition')
		newForGitBasedProjectNamed: projectName2)
		comment:
				'This is a class extension project created in memory first, then written to disk. Uses a separate symbol dictionary from project 1';
		addPackagesNamed: #('Simple-Extensions');
		defaultSymbolDictName: self _symbolDictionaryName2;
		yourself.

	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName2.

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

	packageDefinition2 := projectDefinition2 packageNamed: 'Simple-Extensions'.
	packageDefinition2 addClassExtensionDefinition: classExtensionDefinition.
	self
		should: [ 
			"class extensions need to be made by packages loaded into the symbol dictionary in which the class is defined"
			projectTools load loadProjectDefinition: projectDefinition2 ]
		raise: Error
]

{ #category : 'tests' }
RwProjectToolTest >> testProjectClassExtensionsInSessionMethods [

	"This test attempts to add extension methods to a class that is not in the dictionary that the package is being loaded into ... and the extension methods are kept in session methods in the package's symbol dictionary ..."

	| projectName projectDefinition projectTools classDefinition1 classDefinition2 packageDefinition1 packageDefinition2 className1 className2 testClass1 testInstance1 testClass2 testInstance2 classExtensionDefinition packageNames |
	projectName := 'Simple'.
	packageNames := #('Simple-Core' 'Simple-Extensions' 'Simple-Tests').
	projectTools := Rowan projectTools.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	projectDefinition := ((Rowan globalNamed: 'RwProjectDefinition')
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

	self assert: (testClass1 perform: #civar1) == 1.
	self assert: (testInstance1 perform: #ivar1) isNil.
	testClass2 := Rowan globalNamed: className2.
	self assert: testClass2 notNil.
	testInstance2 := testClass2 new.
	self assert: (testInstance2 perform: #foo)
]

{ #category : 'tests' }
RwProjectToolTest >> testProjectGlobalsClassesExtensionsInSessionMethods [

	"This test attempts to add extension methods to a class is in Globals --- a symbol dictionary owned by another user ... and the extension methods are kept in session methods in the package's symbol dictionary ..."

	| projectName projectDefinition projectTools packageDefinition className classExtensionDefinition packageNames |
	projectName := 'Simple'.
	packageNames := #('Simple-Globals-Extensions').
	projectTools := Rowan projectTools.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	projectDefinition := ((Rowan globalNamed: 'RwProjectDefinition')
		newForGitBasedProjectNamed: projectName)
		comment:
				'This is a simple project created in memory first, then written to disk.';
		packageNames: packageNames;
		defaultSymbolDictName: self _symbolDictionaryName;
		setUseSessionMethodsForExtensions: true
			forPackageNamed: 'Simple-Globals-Extensions';
		yourself.

	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName.

	className := 'Object'.

	classExtensionDefinition := RwClassExtensionDefinition
		newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'instanceFoo'
						protocol: 'accessing'
						source: 'instanceFoo ^''foo''');
		addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'classFoo'
						protocol: 'accessing'
						source: 'classFoo ^''bar''');
		yourself.

	packageDefinition := projectDefinition
		packageNamed: 'Simple-Globals-Extensions'.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	projectTools load loadProjectDefinition: projectDefinition.

	self assert: (Object perform: #classFoo) = 'bar'.
	self assert: (Object new perform: #instanceFoo) = 'foo'
]
