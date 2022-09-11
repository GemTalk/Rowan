Extension { #name : 'RwProjectTest' }

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwProjectTest >> _simpleCreateDefinedProjectForRepoType: repoType [
	| projectName componentName1 packageName1 className1 componentName2 packageName2 className2 definedProject projectsHome |
	projectName := 'ExampleProject'.
	componentName1 := 'Core'.
	packageName1 := projectName , '-' , componentName1.
	className1 := projectName , 'Class'.

	componentName2 := 'tests/Test'.
	packageName2 := projectName , '-Tests'.
	className2 := projectName , 'Test'.

	projectsHome := self _testRowanProjectsSandbox.

	(projectsHome / projectName) ensureDeleteAll.

	definedProject := (Rowan newProjectNamed: projectName)
		projectsHome: projectsHome;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName;
		repoType: repoType;
		addLoadComponentNamed: componentName1;
		addPackagesNamed: {packageName1} toComponentNamed: componentName1;
		addSubcomponentNamed: componentName2 condition: 'tests' comment: 'tests';
		addPackagesNamed: {packageName2} toComponentNamed: componentName2;
		addComponentNamed: componentName2 toComponentNamed: componentName1;
		comment: 'This is a simple project';
		yourself.

	((definedProject packageNamed: packageName1)
		addClassNamed: className1
		super: 'Object'
		instvars: #('ivar')
		category: packageName1
		comment: 'a class')
		addInstanceMethod: 'ivar ^ivar' protocol: 'accessing';
		addInstanceMethod: 'ivar: anObject ivar := anObject' protocol: 'accessing';
		yourself.

	((definedProject packageNamed: packageName2)
		addClassNamed: className2
		super: 'TestCase'
		instvars: #('ivar1')
		category: packageName2
		comment: 'a test')
		addInstanceMethod:
				'testClass
	| object |
	object := ' , className1
						,
							' new.
	object ivar1: 1.
	self assert: ivar1 = 1'
			protocol: 'testing';
		yourself.

	^ definedProject
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwProjectTest >> _simpleCreateResolvedProjectForRepoType: repoType [
	| definedProject |
	definedProject := self _simpleCreateDefinedProjectForRepoType: #'disk'.

	self assert: definedProject class == RwDefinedProject.
	^ definedProject resolveProject
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwProjectTest >> _simpleProjectLifeCycleForRepoType: repoType [
	|  resolvedProject1 resolvedProject2 x projectName packageName2 |
	resolvedProject1 := self _simpleCreateResolvedProjectForRepoType: repoType.

	projectName :=  resolvedProject1 name.
	packageName2 := projectName , '-Tests'.

	self assert: resolvedProject1 class == RwResolvedFromDefinedProject.
	self assert: (x := resolvedProject1 repositoryRoot) exists.

	self assert: (resolvedProject1 packageNames includes: packageName2). "packageName2 created and written to disk"

	resolvedProject1 write.

	resolvedProject2 := Rowan
		projectFromUrl:
			'file:' , (resolvedProject1 specsRoot / projectName , 'ston') pathString
		projectsHome: resolvedProject1 projectsHome.

	self assert: resolvedProject2 class == RwResolvedProject.
	self assert: (x := resolvedProject2 repositoryRoot) exists.
	self assert: resolvedProject2 useGit == resolvedProject1 useGit.

	self deny: (resolvedProject2 packageNames includes: packageName2) "packageName2 not read from disk"
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwProjectTest >> test_issue428_loaded_no_disk [

	| projectName  packageName projectDefinition projectSetDefinition  componentName |

	projectName := 'Issue428'.
	packageName := 'Issue428-Extension'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	(self _testRowanProjectsSandbox / projectName) ensureDeleteAll.

"create project"
	componentName := 'Core'.
	projectDefinition := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addLoadComponentNamed: componentName;
		addPackagesNamed: { packageName }
			toComponentNamed: componentName;
		repoType: #none;
		resolveProject;
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
		addLoadComponentNamed: componentName;
		addPackagesNamed: packageNames
			toComponentNamed: componentName;
		packageFormat: 'tonel';
		yourself.

	self
		handleConfirmationDuring: [ 
			projectDefinition 
				resolveProject;
				export;
				exportLoadSpecification;
				load;
				yourself ].

"test existsOnDisk"

	self assert: (RwProject newNamed: projectName) existsOnDisk.
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwProjectTest >> testResolvedProjectAPI [
	| resolvedProject1 projectName resolvedProject2 x packageName2 loadSpecUrl packageName1 |
	resolvedProject1 := self _simpleCreateResolvedProjectForRepoType: #'disk'.

	projectName := resolvedProject1 name.
	packageName1 := projectName , '-Core'.
	packageName2 := projectName , '-Tests'.
	loadSpecUrl := 'file:'
		, (resolvedProject1 specsRoot / projectName , 'ston') pathString.

	self assert: resolvedProject1 class == RwResolvedFromDefinedProject.
	self assert: (x := resolvedProject1 repositoryRoot) exists.

	self assert: (resolvedProject1 packageNames includes: packageName1).	"packageName1 created and written to disk"
	self assert: (resolvedProject1 packageNames includes: packageName2).	"packageName2 created and written to disk"

	resolvedProject1 write.

"tests attribute not specified, so packageName2 is NOT read from disk"
	resolvedProject2 := Rowan
		projectFromUrl: loadSpecUrl
		projectsHome: resolvedProject1 projectsHome.

	self assert: resolvedProject2 class == RwResolvedProject.
	self assert: (x := resolvedProject2 repositoryRoot) exists.
	self assert: resolvedProject2 useGit == resolvedProject1 useGit.

	self assert: (resolvedProject2 packageNames includes: packageName1).
	self deny: (resolvedProject2 packageNames includes: packageName2).

"tests attribute is specified, so packageName2 is read from disk"
	resolvedProject2 := Rowan
		projectFromUrl: loadSpecUrl
		projectsHome: resolvedProject1 projectsHome
		customConditionalAttributes: #('tests').

	self assert: resolvedProject2 class == RwResolvedProject.
	self assert: (x := resolvedProject2 repositoryRoot) exists.
	self assert: resolvedProject2 useGit == resolvedProject1 useGit.

	self assert: (resolvedProject2 packageNames includes: packageName1).
	self assert: (resolvedProject2 packageNames includes: packageName2).

"tests attribute is not specified, so packageName2 is notread from disk"
	resolvedProject2 := Rowan
		projectFromUrl: loadSpecUrl
		projectsHome: resolvedProject1 projectsHome
		customConditionalAttributes: #().

	self assert: resolvedProject2 class == RwResolvedProject.
	self assert: (x := resolvedProject2 repositoryRoot) exists.
	self assert: resolvedProject2 useGit == resolvedProject1 useGit.

	self assert: (resolvedProject2 packageNames includes: packageName1).
	self deny: (resolvedProject2 packageNames includes: packageName2)
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwProjectTest >> testSimpleProjectLifeCycle_disk [
	self _simpleProjectLifeCycleForRepoType: #disk
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwProjectTest >> testSimpleProjectLifeCycle_git [
	self _simpleProjectLifeCycleForRepoType: #git
]
