Extension { #name : 'RwProjectTest' }

{ #category : '*rowan-tests-35x' }
RwProjectTest >> test_issue428_loaded_no_disk [

| projectName  packageName project projectSetDefinition componentName |

	projectName := 'Issue428'.
	packageName := 'Issue428-Extension'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	componentName := 'Core'.
	project := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addNewComponentNamed: componentName;
		addPackagesNamed: {packageName} toComponentNamed: componentName;
		yourself.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: project.
	Rowan projectTools load2 loadProjectSetDefinition: projectSetDefinition.

"test existsOnDisk"

	self deny: (RwProject newNamed: projectName) existsOnDisk.
]

{ #category : '*rowan-tests-35x' }
RwProjectTest >> test_issue428_loaded_on_disk [

	| projectName project projectTools packageNames componentName |
	projectName := 'Issue428'.
	packageNames := #('Issue428-Core' 'Issue428-Tests').
	projectTools := Rowan projectTools.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	componentName := 'Core'.
	project := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox / 'rowanSimpleProject';
		packageFormat: 'tonel';
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addNewComponentNamed: componentName;
		addPackagesNamed: packageNames toComponentNamed: componentName;
		yourself.

	self
		handleConfirmationDuring: [ project resolve; export ].

"test existsOnDisk"

	self assert: (RwProject newNamed: projectName) existsOnDisk.
]
