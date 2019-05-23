tests
testProjectClassExtensionsInSeparateSymbolDictionary

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

	projectDefinition1 := (RwProjectDefinition
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

	projectDefinition2 := (RwProjectDefinition
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