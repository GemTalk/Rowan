Class {
	#name : 'RwReconcileToolApiTest',
	#superclass : 'RwBrowserToolTest',
	#category : 'Rowan-Tests-ComponentsV2-OnlyV2'
}

{ #category : 'tests' }
RwReconcileToolApiTest >> testReconcileGlobalExtensionMethods [

	| projectDefinitionSet projectName  packageName1 packageName2 packageName3 projectDefinition 
		classDefinition classExtensionDefinition packageDefinition1 packageDefinition2 packageDefinition3
		className componentName projectsHome |

	projectName := 'GlobalsExtensionMethods'.
	packageName1 := 'GlobalsExtensionMethods-Core1'.
	packageName2 := 'GlobalsExtensionMethods-Extension1'.
	packageName3 := 'GlobalsExtensionMethods-Extension2'.
	className := 'GlobalsExtensionMethods'.
	projectsHome := self _testRowanProjectsSandbox.

	(projectsHome / projectName) ensureDeleteAll.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	componentName := 'Core'.
	projectDefinition := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: projectsHome;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addLoadComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 . packageName3  }
			toComponentNamed: componentName;
		resolveProject;
		yourself.

	packageDefinition1 := projectDefinition packageNamed: packageName1.
	packageDefinition2 := projectDefinition packageNamed: packageName2.
	packageDefinition3 := projectDefinition packageNamed: packageName3.

"packageName1 contents"
	classDefinition := (RwClassDefinition
		newForClassNamed: className
			super: 'Object'
			instvars: #(ivar1)
			classinstvars: #()
			classvars: #()
			category: nil
			comment: ''
			pools: #()
			type: 'normal')
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'foo'
				protocol: 'accessing'
				source: 'foo ^1 ');
		yourself.
	packageDefinition1 
		addClassDefinition: classDefinition.

"packageName2 contents"
	classExtensionDefinition := (RwClassExtensionDefinition newForClassNamed: className)
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'bar'
				protocol: '*', packageName2 asLowercase
				source: 'bar ^1 ');
		yourself.
	packageDefinition2 
		addClassExtensionDefinition: classExtensionDefinition.

	classExtensionDefinition := (RwClassExtensionDefinition newForClassNamed: 'Object')
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'bar'
				protocol: '*', packageName2 asLowercase
				source: 'bar ^1 ');
		addClassMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'bar'
				protocol: '*', packageName2 asLowercase
				source: 'bar ^1 ');
		yourself.
	packageDefinition2 
		addClassExtensionDefinition: classExtensionDefinition.

"packageName3 contents"

	classExtensionDefinition := (RwClassExtensionDefinition newForClassNamed: 'Object')
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'foo'
				protocol: '*', packageName3 asLowercase
				source: 'foo ^1 ');
		addClassMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'foo'
				protocol: '*', packageName3 asLowercase
				source: 'foo ^1 ');
		yourself.
	packageDefinition3 
		addClassExtensionDefinition: classExtensionDefinition.

	classExtensionDefinition := (RwClassExtensionDefinition newForClassNamed: 'ByteArray')
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'foo'
				protocol: '*', packageName3 asLowercase
				source: 'foo ^1 ');
		yourself.
	packageDefinition3 
		addClassExtensionDefinition: classExtensionDefinition.

"reconcile"
	projectDefinitionSet := (RwProjectSetDefinition new)
		addProject: projectDefinition;
		yourself.

	Rowan projectTools reconcileV2
		reconcileGlobalClassExtensionsForProjectDefinitionSet: projectDefinitionSet
		globalsUserId: 'SystemUser'.

"validate"
	{packageName1. packageName2 . packageName3 }
		do: [:packageName |
			self assert: (projectDefinition componentForPackageNamed: packageName) name = componentName.
			self assert: (projectDefinition gemstoneSymbolDictNameForPackageNamed: packageName) 
				= projectDefinition gemstoneDefaultSymbolDictName].

	{packageName2, '-Globals' .  packageName3, '-Globals' }		do: [:packageName |
			self assert: (projectDefinition componentForPackageNamed: packageName) name = componentName.
			self assert: (projectDefinition gemstoneSymbolDictNameForPackageNamed: packageName) 
				= 'Globals'].
]

{ #category : 'tests' }
RwReconcileToolApiTest >> testReconcileGlobalExtensionMethods_issue_290 [

	"https://github.com/dalehenrich/Rowan/issues/290"

	| projectDefinitionSet projectName  packageName1 packageName2 packageName3 projectDefinition 
		classDefinition classExtensionDefinition packageDefinition1 packageDefinition2 packageDefinition3
		className classFileString componentName projectsHome |

	projectName := 'GlobalsExtensionMethods'.
	packageName1 := 'GlobalsExtensionMethods-Core1'.
	packageName2 := 'GlobalsExtensionMethods-Extension1'.
	packageName3 := 'GlobalsExtensionMethods-Extension2'.
	className := 'GlobalsExtensionMethods'.
	projectsHome := self _testRowanProjectsSandbox.

	(projectsHome / projectName) ensureDeleteAll.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	componentName := 'Core'.
	projectDefinition := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: projectsHome;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addLoadComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 . packageName3  }
			toComponentNamed: componentName;
		resolveProject;
		yourself.

	packageDefinition1 := projectDefinition packageNamed: packageName1.
	packageDefinition2 := projectDefinition packageNamed: packageName2.
	packageDefinition3 := projectDefinition packageNamed: packageName3.

"packageName1 contents"
	classDefinition := (RwClassDefinition
		newForClassNamed: className
			super: 'Object'
			instvars: #(ivar1)
			classinstvars: #()
			classvars: #()
			category: ''
			comment: ''
			pools: #()
			type: 'normal')
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'foo'
				protocol: 'accessing'
				source: 'foo ^1 ');
		yourself.
	packageDefinition1 
		addClassDefinition: classDefinition.

"packageName2 contents"
	classExtensionDefinition := (RwClassExtensionDefinition newForClassNamed: className)
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'bar'
				protocol: '*', packageName2 asLowercase
				source: 'bar ^1 ');
		addClassMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'bar'
				protocol: 'accessing'
				source: 'bar ^1 ');
		yourself.
	packageDefinition2 
		addClassExtensionDefinition: classExtensionDefinition.

	classExtensionDefinition := (RwClassExtensionDefinition newForClassNamed: 'Object')
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'bar'
				protocol: '*', packageName2 asLowercase
				source: 'bar ^1 ');
		addClassMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'bar'
				protocol: '*', packageName2 asLowercase
				source: 'bar ^1 ');
		yourself.
	packageDefinition2 
		addClassExtensionDefinition: classExtensionDefinition.

"packageName3 contents"

	classExtensionDefinition := (RwClassExtensionDefinition newForClassNamed: 'Object')
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'foo'
				protocol: '*', packageName3 asLowercase
				source: 'foo ^1 ');
		addClassMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'foo'
				protocol: '*', packageName3 asLowercase
				source: 'foo ^1 ');
		yourself.
	packageDefinition3 
		addClassExtensionDefinition: classExtensionDefinition.

	classExtensionDefinition := (RwClassExtensionDefinition newForClassNamed: 'ByteArray')
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'foo'
				protocol: '*', packageName3 asLowercase
				source: 'foo ^1 ');
		yourself.
	packageDefinition3 
		addClassExtensionDefinition: classExtensionDefinition.

"reconcile"
	projectDefinitionSet := (RwProjectSetDefinition new)
		addProject: projectDefinition;
		yourself.

	Rowan projectTools reconcileV2
		reconcileGlobalClassExtensionsForProjectDefinitionSet: projectDefinitionSet
		globalsUserId: 'SystemUser'.

	projectDefinition
		export;
		exportLoadSpecification;
		yourself.

"validate"
	{packageName1. packageName2 . packageName3 }
		do: [:packageName |
			self assert: (projectDefinition componentForPackageNamed: packageName) name = componentName.
			self assert: (projectDefinition gemstoneSymbolDictNameForPackageNamed: packageName) 
				= projectDefinition gemstoneDefaultSymbolDictName].

	{packageName2, '-Globals' .  packageName3, '-Globals' }		do: [:packageName |
			self assert: (projectDefinition componentForPackageNamed: packageName) name = componentName.
			self assert: (projectDefinition gemstoneSymbolDictNameForPackageNamed: packageName) 
				= 'Globals'].
	 ((projectsHome / projectName) files 
		do: [:tonelFile | 
			tonelFile readStreamDo: [:fileStream | classFileString := fileStream contents ].
			self deny: (classFileString includesString: 'classSide') ]
]

{ #category : 'tests' }
RwReconcileToolApiTest >> testReconcileProjectSetMethods [

	| projectDefinitionSet projectName  packageName1 packageName2 packageName3 projectDefinition 
		classExtensionDefinition packageDefinition1 packageDefinition2 packageDefinition3 componentName
		projectsHome x |

	projectName := 'GlobalsExtensionMethods'.
	packageName1 := 'GlobalsExtensionMethods-Core1'.
	packageName2 := 'GlobalsExtensionMethods-Extension1'.
	packageName3 := 'GlobalsExtensionMethods-Extension2'.
	projectsHome := self _testRowanProjectsSandbox.

	(projectsHome / projectName) ensureDeleteAll.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	componentName := 'Core'.
	projectDefinition := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: projectsHome;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addLoadComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 . packageName3  }
			toComponentNamed: componentName;
		resolveProject;
		yourself.

	packageDefinition1 := projectDefinition packageNamed: packageName1.
	packageDefinition2 := projectDefinition packageNamed: packageName2.
	packageDefinition3 := projectDefinition packageNamed: packageName3.

"packageName1 contents"

"packageName2 contents"
	classExtensionDefinition := (RwClassExtensionDefinition newForClassNamed: 'Object')
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'bar'
				protocol: '*', packageName2 asLowercase
				source: 'bar ^1 ');
		addClassMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'bar'
				protocol: '*', packageName2 asLowercase
				source: 'bar ^1 ');
		yourself.
	packageDefinition2 
		addClassExtensionDefinition: classExtensionDefinition.

"packageName3 contents"

	classExtensionDefinition := (RwClassExtensionDefinition newForClassNamed: 'Object')
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'foo'
				protocol: '*', packageName3 asLowercase
				source: 'foo ^1 ');
		addClassMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'foo'
				protocol: '*', packageName3 asLowercase
				source: 'foo ^1 ');
		yourself.
	packageDefinition3 
		addClassExtensionDefinition: classExtensionDefinition.

	classExtensionDefinition := (RwClassExtensionDefinition newForClassNamed: 'ByteArray')
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'foo'
				protocol: '*', packageName3 asLowercase
				source: 'foo ^1 ');
		yourself.
	packageDefinition3 
		addClassExtensionDefinition: classExtensionDefinition.

"reconcile"
	projectDefinitionSet := (RwProjectSetDefinition new)
		addProject: projectDefinition;
		yourself.

	Rowan projectTools reconcileV2
		reconcileProjectDefinitionSet: projectDefinitionSet.

"validate"
	{packageName1. packageName2 . packageName3 }
		do: [:packageName |
			self assert: (projectDefinition componentForPackageNamed: packageName) name = componentName.
			self assert: (projectDefinition gemstoneSymbolDictNameForPackageNamed: packageName) 
				= projectDefinition gemstoneDefaultSymbolDictName].

	{packageName2, '-ext-Globals' .  packageName3, '-ext-Globals' }		do: [:packageName |
			self assert: (projectDefinition componentForPackageNamed: packageName) name = componentName.
			self assert: (x := projectDefinition gemstoneSymbolDictNameForPackageNamed: packageName) 
				= 'Globals'].
]
