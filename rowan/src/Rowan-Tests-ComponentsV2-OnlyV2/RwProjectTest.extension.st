Extension { #name : 'RwProjectTest' }

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwProjectTest >> test_issue428_loaded_no_disk [

	| projectName  packageName projectDefinition projectSetDefinition  componentName |

	projectName := 'Issue428'.
	packageName := 'Issue428-Extension'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	componentName := 'Core'.
	projectDefinition := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addNewComponentNamed: componentName;
		addPackagesNamed: { packageName }
			toComponentNamed: componentName;
		repoType: #none;
		resolve;
		yourself.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"test existsOnDisk"

	self deny: (RwProject newNamed: projectName) existsOnDisk.
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwProjectTest >> test_issue428_loaded_on_disk [

	| projectName projectDefinition projectTools packageNames componentName |
	projectName := 'Issue428'.
	packageNames := #('Issue428-Core' 'Issue428-Tests').
	projectTools := Rowan projectTools.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	componentName := 'Core'.
	projectDefinition := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox  / 'rowanSimpleProject';
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addNewComponentNamed: componentName;
		addPackagesNamed: packageNames
			toComponentNamed: componentName;
		packageFormat: 'tonel';
		yourself.

	self
		handleConfirmationDuring: [ 
			projectDefinition 
				resolve;
				export;
				exportLoadSpecification;
				load;
				yourself ].

"test existsOnDisk"

	self assert: (RwProject newNamed: projectName) existsOnDisk.
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwProjectTest >> testProjectLifeCycle [
	| projectName componentName packageName definedProject className resolvedProject1 resolvedProject2 x projectsHome |
	projectName := 'ExampleProject'.
	componentName := 'Core'.
	packageName := projectName , '-' , componentName.
	className := projectName , 'Class'.
	projectsHome := self _testRowanProjectsSandbox.

	(projectsHome / projectName) ensureDeleteAll.

	definedProject := (Rowan newProjectNamed: projectName)
		projectsHome: projectsHome;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName;
		repoType: #'disk';
		addTopLevelComponentNamed: componentName;
		addPackagesNamed: {packageName} toComponentNamed: componentName;
		yourself.

	((definedProject packageNamed: packageName)
		addClassNamed: className
		super: 'Object'
		instvars: #('ivar')
		category: packageName
		comment: 'a class')
		addInstanceMethod: 'ivar ^ivar' protocol: 'accessing';
		addInstanceMethod: 'ivar: anObject ivar := anObject' protocol: 'accessing';
		yourself.

	self assert: definedProject class == RwDefinedProject.
	resolvedProject1 := definedProject resolve.

	self assert: resolvedProject1 class == RwResolvedProject.
	self assert: (x := resolvedProject1 repositoryRoot) exists.

	resolvedProject1 write.

	resolvedProject2 := Rowan
		projectFromUrl:
			'file:' , (resolvedProject1 specsRoot / projectName , 'ston') pathString
		projectsHome: projectsHome.

	self assert: resolvedProject2 class == RwResolvedProject.
	self assert: (x := resolvedProject2 repositoryRoot) exists.
]
