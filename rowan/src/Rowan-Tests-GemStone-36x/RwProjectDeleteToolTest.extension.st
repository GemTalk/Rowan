Extension { #name : 'RwProjectDeleteToolTest' }

{ #category : '*rowan-tests-gemstone-36x' }
RwProjectDeleteToolTest >> testDeleteProjectWithExtensionMethods [


	| class projectName1 projectName2 packageName1 packageName2 className1 className2 classDefinition
		symDict registry projectDefinition1 projectDefinition2 packageDefinition projectNames 
		classExtensionDefinition projectSetDefinition componentName project1 project2 audit |

	projectName1 := 'DeleteProject'.
	projectName2 := 'ExtensionProject'.
	packageName1 := 'DeleteProject-Core'.
	packageName2 := 'ExtensionProject-Core'.
	className1 := 'DeleteProjectClass'.
	className2 := 'ExtensionProjectClass'.
	projectNames := {projectName1 . projectName2 }.

	projectNames do: [:projectName | 
		(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
			ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

"create projects"
	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName1;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addLoadComponentNamed: componentName;
		addPackagesNamed: { packageName1 } toComponentNamed: componentName;
		resolve;
		yourself.

	projectDefinition2 := RwResolvedProjectV2 new
		projectName: projectName2;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addLoadComponentNamed: componentName;
		addPackagesNamed: { packageName2 } toComponentNamed: componentName;
		resolve;
		yourself.

"create classes and methods for project1"
	classDefinition := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className2.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: '*', packageName1
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition 
		addClassDefinition: classDefinition;
		addClassExtensionDefinition: classExtensionDefinition.

"create classes and methods for project2"
	classDefinition := RwClassDefinition
		newForClassNamed: className2
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName2
		comment: ''
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition 
		addClassDefinition: classDefinition.

"load project definitions"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition 
		addDefinition: projectDefinition1;
		addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	project1 := Rowan projectNamed: projectName1.
	self assert: (audit := project1 audit) isEmpty.
	project2 := Rowan projectNamed: projectName2.
	self assert: (audit := project2 audit) isEmpty.

"validation"
	class := Rowan globalNamed: className1.
	self assert: class rowanPackageName = packageName1.
	self assert: class category= packageName1.
	class := Rowan globalNamed: className2.
	self assert: class rowanPackageName = packageName2.
	self assert: class category= packageName2.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.

"delete project"
	Rowan projectTools delete deleteProjectNamed: projectName1.

"validation"
	class := Rowan globalNamed: className1.
	self assert: class isNil.
	class := Rowan globalNamed: className2.
	self assert: class rowanPackageName = packageName2.
	self assert: class category= packageName2.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project2 audit) isEmpty.
	self assert: registry packageRegistry size = 1.
]

{ #category : '*rowan-tests-gemstone-36x' }
RwProjectDeleteToolTest >> testDeleteProjectWithExtensionMethods_345 [

	"https://github.com/dalehenrich/Rowan/issues/342"

	| class projectName1 packageName1 packageName2 className1 classDefinition
		symDict registry projectDefinition1 packageDefinition projectNames classExtensionDefinition
		projectSetDefinition componentName project audit |

	projectName1 := 'DeleteProject'.
	packageName1 := 'DeleteProject-Core'.
	packageName2 := 'DeleteProject-Extension'.
	className1 := 'DeleteProjectClass'.
	projectNames := {projectName1  }.

	projectNames do: [:projectName | 
		(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"create projects"
	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName1;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addLoadComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 } toComponentNamed: componentName;
		resolve;
		yourself.

"create classes and methods for project1"
	classDefinition := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition 
		addClassDefinition: classDefinition;
		yourself.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: '*', packageName2
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName2.
	packageDefinition 
		addClassExtensionDefinition: classExtensionDefinition.

"load project definitions"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition 
		addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	project := Rowan projectNamed: projectName1.
	self assert: (audit := project audit) isEmpty.

"validation"
	class := Rowan globalNamed: className1.
	self assert: class rowanPackageName = packageName1.
	self assert: class category= packageName1.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.

"delete project"
	Rowan projectTools delete deleteProjectNamed: projectName1.

"validation"
	class := Rowan globalNamed: className1.
	self assert: class isNil.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self deny: project isLoaded.
	self assert: registry packageRegistry size = 0.
]

{ #category : '*rowan-tests-gemstone-36x' }
RwProjectDeleteToolTest >> testDeleteSimpleProject [

	| class projectName packageName className classDefinition symDict registry projectDefinition 
		packageDefinition componentName project audit |

	projectName := 'DeleteProject'.
	packageName := 'DeleteProject-Core'.
	className := 'DeleteProjectClass'.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"create project"
	componentName := 'Core'.
	projectDefinition := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addLoadComponentNamed: componentName;
		addPackagesNamed: { packageName } toComponentNamed: componentName;
		resolve;
		yourself.

"create classes and methods"
	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition addClassDefinition: classDefinition.

"load project definition"
	Rowan projectTools load loadProjectDefinition: projectDefinition.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

"validation"
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName.
	self assert: class category= packageName.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.

"delete project"
	Rowan projectTools delete deleteProjectNamed: projectName.

"validation"
	class := Rowan globalNamed: className.
	self assert: class isNil.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 0.
]

{ #category : '*rowan-tests-gemstone-36x' }
RwProjectDeleteToolTest >> testForceDeleteProjectWithExtensionMethods [


	| class projectName1 projectName2 packageName1 packageName2 className1 className2 classDefinition
		symDict registry projectDefinition1 projectDefinition2 packageDefinition projectNames 
		classExtensionDefinition projectSetDefinition componentName project1 project2 audit |

	projectName1 := 'DeleteProject'.
	projectName2 := 'ExtensionProject'.
	packageName1 := 'DeleteProject-Core'.
	packageName2 := 'ExtensionProject-Core'.
	className1 := 'DeleteProjectClass'.
	className2 := 'ExtensionProjectClass'.
	projectNames := {projectName1 . projectName2 }.

	projectNames do: [:projectName | 
		(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
			ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

"create projects"
	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName1;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addLoadComponentNamed: componentName;
		addPackagesNamed: { packageName1 } toComponentNamed: componentName;
		resolve;
		yourself.
	projectDefinition2 := RwResolvedProjectV2 new
		projectName: projectName2;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addLoadComponentNamed: componentName;
		addPackagesNamed: { packageName2 } toComponentNamed: componentName;
		resolve;
		yourself.

"create classes and methods for project1"
	classDefinition := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className2.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: '*', packageName1
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition 
		addClassDefinition: classDefinition;
		addClassExtensionDefinition: classExtensionDefinition.

"create classes and methods for project2"
	classDefinition := RwClassDefinition
		newForClassNamed: className2
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName2
		comment: ''
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition 
		addClassDefinition: classDefinition.

"load project definitions"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition 
		addDefinition: projectDefinition1;
		addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	project1 := Rowan projectNamed: projectName1.
	self assert: (audit := project1 audit) isEmpty.
	project2 := Rowan projectNamed: projectName2.
	self assert: (audit := project2 audit) isEmpty.

"validation"
	class := Rowan globalNamed: className1.
	self assert: class rowanPackageName = packageName1.
	self assert: class category= packageName1.
	class := Rowan globalNamed: className2.
	self assert: class rowanPackageName = packageName2.
	self assert: class category= packageName2.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.

"delete project"
	Rowan projectTools delete forceDeleteProjectNamed: projectName1.

"validation"
	class := Rowan globalNamed: className1.
	self assert: class isNil.
	class := Rowan globalNamed: className2.
	self assert: class rowanPackageName = packageName2.
	self assert: class category= packageName2.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project2 audit) isEmpty.
	self assert: registry packageRegistry size = 1.
]

{ #category : '*rowan-tests-gemstone-36x' }
RwProjectDeleteToolTest >> testForceDeleteSimpleProject [

	| class projectName packageName className classDefinition symDict registry projectDefinition 
		packageDefinition componentName project audit |

	projectName := 'DeleteProject'.
	packageName := 'DeleteProject-Core'.
	className := 'DeleteProjectClass'.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"create project"
	componentName := 'Core'.
	projectDefinition:= RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addLoadComponentNamed: componentName;
		addPackagesNamed: { packageName } toComponentNamed: componentName;
		resolve;
		yourself.

"create classes and methods"
	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition addClassDefinition: classDefinition.

"load project definition"
	Rowan projectTools load loadProjectDefinition: projectDefinition.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

"validation"
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName.
	self assert: class category= packageName.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.

"delete project"
	Rowan projectTools delete forceDeleteProjectNamed: projectName.

"validation"
	class := Rowan globalNamed: className.
	self assert: class isNil.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 0.
]
