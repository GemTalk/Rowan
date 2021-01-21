Extension { #name : 'RwRowanProjectIssuesTest' }

{ #category : '*rowan-tests-gemstone-36x' }
RwRowanProjectIssuesTest >> testIssue208_move_method_from_project_to_project [

	"create class and method in same project/package"

	"move class to one package in new project and method to another package in new project"

	"simulating extension method moves during an adopt"

	"simplification of RwRowanSample4Test>>testIssue208_adopt_load"

	| projectName1 projectName2 packageName1 packageName2 packageName3 className 
		projectDefinition1 project1 project2 audit
		classDefinition packageDefinition projectSetDefinition projectDefinition2 classExtensionDefinition class 
		ar symDict registry oldClass componentName |
	projectName1 := 'Issue208_moveMethodBetweenProjects1'.
	projectName2 := 'Issue208_moveMethodBetweenProjects2'.
	packageName1 := 'Issue208-Core1'.
	packageName2 := 'Issue208-Core2'.
	packageName3 := 'Issue208-Extension2'.
	className := 'Issue208Class'.

	{projectName1. projectName2}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project 1"
	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName1;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addLoadComponentNamed: componentName;
		addPackagesNamed: { packageName1 }
			toComponentNamed: componentName;
		resolve;
		yourself.

	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

"load project1"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validation"
	project1 := Rowan projectNamed: projectName1.
	self assert: (audit := project1 audit) isEmpty.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName1.
	self assert: (class new perform: #method1) = 1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName1 asSymbol.

"create project2"
	projectDefinition2 := RwResolvedProjectV2 new
		projectName: projectName2;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName2;
		addLoadComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 . packageName3 }
			toComponentNamed: componentName;
		resolve;
		yourself.

	packageDefinition := projectDefinition2 packageNamed: packageName2.

	classDefinition 
		category: packageName2;
		removeInstanceMethod: #method1.
	packageDefinition addClassDefinition: classDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName3.
	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: '*', packageName3 asLowercase
					source: 'method1 ^1').
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

"load project2"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validation"
	project2 := Rowan projectNamed: projectName2.
	self assert: (audit := project1 audit) isEmpty.
	self assert: (audit := project2 audit) isEmpty.
	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: oldClass == class.
	self assert: class rowanPackageName = packageName2.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName3.
	self assert: (class new perform: #method1) = 1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2 asSymbol.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
]
