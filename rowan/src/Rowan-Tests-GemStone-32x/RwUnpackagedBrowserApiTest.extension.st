Extension { #name : 'RwUnpackagedBrowserApiTest' }

{ #category : '*rowan-tests-gemstone-32x' }
RwUnpackagedBrowserApiTest >> testIssue420_A [

	"https://github.com/GemTalk/Rowan/issues/420"

	"move method from packaged category to another packaged category (different package) (class is unpackaged)"

	| projectName  packageName1 packageName2 resolvedProject projectSetDefinition audit testClass symDict registry 
		componentName project |

	projectName := 'Issue364'.
	packageName1 := 'Issue364-Extension-1'.
	packageName2 := 'Issue364-Extension-2'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	componentName := 'Core'.
	resolvedProject := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: {packageName1 . packageName2} toComponentNamed: componentName;
		yourself.

	resolvedProject resolve.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: resolvedProject.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

	testClass := self _testClass.
	testClass addCategory: '*' , packageName1 asLowercase.
	testClass
		rwCompileMethod: 'bar ^''bar'''
		category: '*' , packageName1 asLowercase.

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = ('*' , packageName1 asLowercase) asSymbol.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project audit) isEmpty.
	self assert: registry packageRegistry size = 2.
false
ifFalse: [ 
	self assert: registry methodRegistry size = 1].

"move"
	testClass addCategory: '*' , packageName2 asLowercase.
	testClass rwMoveMethod: #bar toCategory: '*' , packageName2 asLowercase.

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = ('*' , packageName2 asLowercase) asSymbol.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project audit) isEmpty.
	self assert: registry packageRegistry size = 2.
false
ifFalse: [ 
	self assert: registry methodRegistry size = 1].

"load -- effectively unload extension methods"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: resolvedProject.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) isNil.

"audit"
	self assert: (audit := project audit) isEmpty.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
false
ifFalse: [ 
	self assert: registry methodRegistry size = 0].
]

{ #category : '*rowan-tests-gemstone-32x' }
RwUnpackagedBrowserApiTest >> testIssue420_B [

	"https://github.com/GemTalk/Rowan/issues/420"

	"move method from extension category to packaged category (class is packaged)"

	| projectName  packageName1 packageName2 projectDefinition projectSetDefinition audit testClass symDict registry 
		componentName project |

	projectName := 'Issue364'.
	packageName1 := 'Issue364-Core-1'.
	packageName2 := 'Issue364-Extension-2'.

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
		addComponentNamed: componentName;
		addPackagesNamed: {packageName1 . packageName2} toComponentNamed: componentName;
		resolve;
		yourself.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

	testClass := Object rwSubclass: self _testPackagedClassName
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	testClass addCategory: '*' , packageName2 asLowercase.
	testClass
		rwCompileMethod: 'bar ^''bar'''
		category: '*' , packageName2 asLowercase.

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = ('*' , packageName2 asLowercase) asSymbol.
"registry check"
	self assert: (audit := project audit) isEmpty.
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
false
ifFalse: [ 
	self assert: registry methodRegistry size = 1].

"move"
	testClass addCategory: 'accessing'.
	testClass rwMoveMethod: #bar toCategory: 'accessing'.

"validate"
	self assert: (audit := project audit) isEmpty.
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = #'accessing'.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
false
ifFalse: [ 
	self assert: registry methodRegistry size = 1].

"load -- effectively unload extension methods"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	self assert: (Rowan globalNamed: self _testPackagedClassName) isNil.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.

"registry check"
	self assert: (audit := project audit) isEmpty.
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
false
ifFalse: [ 
	self assert: registry methodRegistry size = 0].
]

{ #category : '*rowan-tests-gemstone-32x' }
RwUnpackagedBrowserApiTest >> testIssue420_C [

	"https://github.com/GemTalk/Rowan/issues/420"

	"move method from packaged category to unpackaged category (class is unpackaged)"

	| projectName  packageName1 packageName2 projectDefinition projectSetDefinition audit testClass symDict registry
		componentName project |

	projectName := 'Issue364'.
	packageName1 := 'Issue364-Extension-1'.
	packageName2 := 'Issue364-Extension-2'.

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
		addComponentNamed: componentName;
		addPackagesNamed: {packageName1 . packageName2} toComponentNamed: componentName;
		resolve;
		yourself.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

	testClass := self _testClass.
	testClass addCategory: '*' , packageName1 asLowercase.
	testClass
		rwCompileMethod: 'bar ^''bar'''
		category: '*' , packageName1 asLowercase.

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = ('*' , packageName1 asLowercase) asSymbol.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project audit) isEmpty.
	self assert: registry packageRegistry size = 2.
false
ifFalse: [ 
	self assert: registry methodRegistry size = 1].

"move"
	testClass addCategory: 'accessing'.
	[ testClass rwMoveMethod: #bar toCategory:  'accessing' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = 'accessing'  asSymbol.
"audit"
	self assert: (audit := project audit) isEmpty.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
false
ifFalse: [ 
	self assert: registry methodRegistry size = 0].

"load -- effectively unload extension methods"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	[ testClass rwRemoveSelector: #bar ]
			on: RwPerformingUnpackagedEditNotification
			do: [:ex | ex resume ].	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) isNil.

"audit"
	self assert: (audit := project audit) isEmpty.
]

{ #category : '*rowan-tests-gemstone-32x' }
RwUnpackagedBrowserApiTest >> testIssue420_D [

	"https://github.com/GemTalk/Rowan/issues/420"

	"move method from unpackaged category to packaged category (class is unpackaged)"

	| projectName  packageName1 projectDefinition projectSetDefinition audit testClass symDict registry 
		componentName project |

	projectName := 'Issue364'.
	packageName1 := 'Issue364-Extension-1'.

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
		addComponentNamed: componentName;
		addPackagesNamed: {packageName1} toComponentNamed: componentName;
		resolve;
		yourself.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

	testClass := self _testClass.
	testClass addCategory: 'accessing'.
	[ testClass
		rwCompileMethod: 'bar ^''bar'''
		category: 'accessing' ]
			on: RwPerformingUnpackagedEditNotification
			do: [:ex | ex resume ].	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = 'accessing' asSymbol.
"audit"
	self assert: (audit := project audit) isEmpty.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
false
ifFalse: [ 
	self assert: registry methodRegistry size = 0].

"move"
	testClass addCategory: '*' , packageName1 asLowercase.
	testClass rwMoveMethod: #bar toCategory: '*' , packageName1 asLowercase.	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = ('*' , packageName1 asLowercase) asSymbol.
"audit"
	self assert: (audit := project audit) isEmpty.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
false
ifFalse: [ 
	self assert: registry methodRegistry size = 1].

"load -- effectively unload extension methods"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) isNil.
"audit"
	self assert: (audit := project audit) isEmpty.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
false
ifFalse: [ 
	self assert: registry methodRegistry size = 0].
]

{ #category : '*rowan-tests-gemstone-32x' }
RwUnpackagedBrowserApiTest >> testIssue420_E [

	"https://github.com/GemTalk/Rowan/issues/420"

	"move method from unpackaged category to category in another unpackaged category (class is unpackaged)"

	| projectName  packageName1 projectDefinition projectSetDefinition testClass symDict registry 
	componentName project audit |

	projectName := 'Issue364'.
	packageName1 := 'Issue364-Extension-1'.

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
		addComponentNamed: componentName;
		addPackagesNamed: {packageName1} toComponentNamed: componentName;
		resolve;
		yourself.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	testClass := self _testClass.
	testClass addCategory: 'accessing'.
	[ testClass
		rwCompileMethod: 'bar ^''bar'''
		category: 'accessing' ]
			on: RwPerformingUnpackagedEditNotification
			do: [:ex | ex resume ].	

"validate"
	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = 'accessing' asSymbol.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
false
ifFalse: [ 
	self assert: registry methodRegistry size = 0].

"move"
	testClass addCategory: 'other accessing'.
	testClass rwMoveMethod: #bar toCategory: 'other accessing'.	

"validate"
	self assert: (audit := project audit) isEmpty.
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = #'other accessing'.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
false
ifFalse: [ 
	self assert: registry methodRegistry size = 0].

"load -- effectively unload extension methods"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	self assert: (audit := project audit) isEmpty.
	[ testClass rwRemoveSelector: #bar ]
			on: RwPerformingUnpackagedEditNotification
			do: [:ex | ex resume ].	
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) isNil.

"audit"
	self assert: (audit := project audit) isEmpty.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
false
ifFalse: [ 
	self assert: registry methodRegistry size = 0].
]
