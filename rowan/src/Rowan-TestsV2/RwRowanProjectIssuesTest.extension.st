Extension { #name : 'RwRowanProjectIssuesTest' }

{ #category : '*rowan-testsv2' }
RwRowanProjectIssuesTest >> testIssue495_move_class_and_extension_method_to_new_symbol_dict [

	"Port of RwRowanProjectIssuesTest debug: #testIssue215_move_class_and_extension_method_to_new_symbol_dict
		to V2 api and reproduce Issue #495"

	"https://github.com/dalehenrich/Rowan/issues/495"

	| projectName  packageName1 packageName2 packageName3 project1 project2 
		classDefinition packageDefinition className1 className2 class projectSetDefinition
		classExtensionDefinition oldClass project audit |
	projectName := 'Issue215'.
	packageName1 := 'Issue215-Core1'.
	packageName2 := 'Issue215-Tools'.
	packageName3 := 'Issue215-Tools-Extensions'.
	className1 := 'Issue215Class1'.
	className2 := 'Issue215Class2'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	project1 := RwResolvedProjectV2 new
		projectName: projectName;
		addNewComponentNamed: 'Core';
		addPackageNamed: packageName1 
			toComponentNamed: 'Core';
		addPackageNamed: packageName2 
			toComponentNamed: 'Core';
		gemstoneSetSymbolDictName: self _symbolDictionaryName1
			forPackageNamed: packageName1;
		gemstoneSetSymbolDictName: self _symbolDictionaryName1
			forPackageNamed: packageName2;
		yourself.

	packageDefinition := project1 packageNamed: packageName1.
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
	packageDefinition addClassDefinition: classDefinition.

	packageDefinition := project1 packageNamed: packageName2.
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
	packageDefinition addClassDefinition: classDefinition.

	"create extension method in different package"
	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'mover'
				protocol: '*', packageName2 asLowercase
				source: 'mover ^2').
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: project1.
	Rowan projectTools loadV2 loadProjectSetDefinition: projectSetDefinition.

	"validate"
	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	class := Rowan globalNamed: className1.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName2.
	self assert: (class new perform: #mover) = 2.

	"move the class to different symbol dictionary and move extension methods to new package"
	project2 := (Rowan projectNamed: projectName) asDefinition.
	project2
		addPackageNamed: packageName3 
			toComponentNamed: 'Core' ;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2
			forPackageNamed: packageName2;
		gemstoneSetSymbolDictName: self _symbolDictionaryName1
			forPackageNamed: packageName3.

	packageDefinition := project2 packageNamed: packageName2.
	(packageDefinition classExtensions at: className1) removeInstanceMethod: #mover.

	packageDefinition := project2 packageNamed: packageName3.
	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'mover'
				protocol: '*', packageName3 asLowercase
				source: 'mover ^2').
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.
	
	"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: project2.
	Rowan projectTools loadV2 loadProjectSetDefinition: projectSetDefinition.

	"validate"
	self assert: (audit := project audit) isEmpty.
	oldClass := class.
	class := Rowan globalNamed: className1.
	self assert: class == oldClass.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName3.
	self assert: (class new perform: #mover) = 2.
]

{ #category : '*rowan-testsv2' }
RwRowanProjectIssuesTest >> testIssue582 [

	"
	Copy of RwProjectSetTest>>testProjectAdoptTool_existing_project in package Rowan-Tests-ComponentsV2-OnlyV2 ... reproduces a V2 bug.
		Basically the component structure is copied over when the loaded project is first created and not updated subsequently. Also it seems that
		the component dictionaries are losing the original data and that I suspect is a side-effect of an incomplete copy ... all suspicions at this point
		so I need to get this figured out, since it is a pretty gross bug.
	"

	"Create project definition structure (2 projects), build classes and methods using traditional methods, 
		then adopt the structure"

	"test for RwPkgCreateTool>>createLoadedPackageNamed:inProjectNamed:"

	| projectTools projectName1 projectName2 projectDefinition1 projectDefinition2 projectSetDefinition 
		projectNames classDefinition packageNames1 packageNames2 classNames1 classNames2 
		classExtensionDefinition symDict1 symDict2 symbolList theClass loadedProjectSet diff classComment 
		componentName project1 project2 |

	projectTools := Rowan projectTools.
	projectName1 := 'AdoptProject1'.
	projectName2 := 'AdoptProject2'.
	projectNames := {projectName1.
		projectName2}.
	packageNames1 := #('Adopt1-Core' 'Adopt1-Extensions').
	packageNames2 := #('Adopt2-Core' 'Adopt2-Extensions').
	classNames1 := #('Adopt1Class1').
	classNames2 := #('Adopt2Class1').
	classComment := ''.

	projectNames
		do: [ :projectName | 
			(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName1.
	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName2.

	projectSetDefinition := RwProjectSetDefinition new.

	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName1;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addNewComponentNamed: componentName;
		resolve;
		yourself.
	projectSetDefinition addDefinition: projectDefinition1.

	projectDefinition2 := RwResolvedProjectV2 new
		projectName: projectName2;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName2;
		addNewComponentNamed: componentName;
		resolve;
		yourself.
	projectSetDefinition addDefinition: projectDefinition2.

	"create projects with no packages"
	projectTools loadV2 loadProjectSetDefinition: projectSetDefinition. "create loaded project and empty loaded packages"

	"add new packages to loaded projects"
	project1 := Rowan projectNamed: projectName1.
	project2 := Rowan projectNamed: projectName2.
	
	(project1 asDefinition)
		addPackagesNamed: packageNames1 toComponentNamed: componentName;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 
			forPackageNamed: (packageNames1 at: 2);
		load;
		yourself.

	(project2 asDefinition)
		addPackagesNamed: packageNames2 toComponentNamed: componentName;
		gemstoneSetSymbolDictName: self _symbolDictionaryName1 
			forPackageNamed: (packageNames2 at: 2);
		load;
		yourself.

	"construct project definition structures"
	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition1 := project1 asDefinition.
	projectSetDefinition addProject: projectDefinition1.

	classDefinition := RwClassDefinition
		newForClassNamed: (classNames1 at: 1)
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: (packageNames1 at: 1)
		comment: classComment
		pools: #()
		type: 'normal'.
	classDefinition
		gs_symbolDictionary: self _symbolDictionaryName1;
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'instanceFoo'
				protocol: 'accessing'
				source: 'instanceFoo ^true');
		addClassMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'classFoo'
				protocol: 'accessing'
				source: 'classFoo ^true').

	(projectDefinition1 packageNamed: (packageNames1 at: 1))
		addClassDefinition: classDefinition.

	classExtensionDefinition := (RwClassExtensionDefinition
			newForClassNamed: (classNames2 at: 1))
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'instanceExtensionFoo'
						protocol: '*', (packageNames1 at: 2)
						source: 'instanceExtensionFoo ^true');
		addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'classExtensionFoo'
						protocol:'*', (packageNames1 at: 2)
						source: 'classExtensionFoo ^true');		
		yourself.

	(projectDefinition1 packageNamed: (packageNames1 at: 2))
		addClassExtensionDefinition: classExtensionDefinition.

	projectDefinition2 := project2 asDefinition.
	projectSetDefinition addProject: projectDefinition2.

	classDefinition := RwClassDefinition
		newForClassNamed: (classNames2 at: 1)
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: (packageNames2 at: 1)
		comment: classComment
		pools: #()
		type: 'normal'.
	classDefinition
		gs_symbolDictionary: self _symbolDictionaryName2;
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'instanceBar'
				protocol: 'accessing'
				source: 'instanceBar ^true');
		addClassMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'classBar'
				protocol: 'accessing'
				source: 'classBar ^true').

	(projectDefinition2 packageNamed: (packageNames2 at: 1))
		addClassDefinition: classDefinition.

	classExtensionDefinition := (RwClassExtensionDefinition
			newForClassNamed: (classNames1 at: 1))
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'instanceExtensionBar'
						protocol: '*', (packageNames2 at: 2)
						source: 'instanceExtensionBar ^true');
		addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'classExtensionBar'
						protocol:'*', (packageNames2 at: 2)
						source: 'classExtensionBar ^true');		
		yourself.

	(projectDefinition2 packageNamed: (packageNames2 at: 2))
		addClassExtensionDefinition: classExtensionDefinition.

	"construct class structures using traditional methods"
	symbolList := Rowan image symbolList.

	symDict1 := Rowan globalNamed: self _symbolDictionaryName1.
	theClass := Object subclass: (classNames1 at: 1)
			instVarNames: #()
			classVars: #()
			classInstVars: #()
			poolDictionaries: #()
			inDictionary: symDict1
			options: #().
	theClass 
		comment: classComment;
		category: (packageNames1 at: 1).
	theClass
		compileMethod: 'instanceFoo ^true'
			dictionaries: symbolList
			category: #'accessing'
			intoMethodDict: nil
			intoCategories: nil
			environmentId: 0;
		compileMethod: 'instanceExtensionBar ^true'
			dictionaries: symbolList
			category: ('*', (packageNames2 at: 2)) asSymbol
			intoMethodDict: nil
			intoCategories: nil
			environmentId: 0.
	theClass class
		compileMethod: 'classFoo ^true'
			dictionaries: symbolList
			category: #'accessing'
			intoMethodDict: nil
			intoCategories: nil
			environmentId: 0;
		compileMethod: 'classExtensionBar ^true'
			dictionaries: symbolList
			category: ('*', (packageNames2 at: 2)) asSymbol
			intoMethodDict: nil
			intoCategories: nil
			environmentId: 0.

	symDict2 := Rowan globalNamed: self _symbolDictionaryName2.
	theClass := Object subclass: (classNames2 at: 1)
			instVarNames: #()
			classVars: #()
			classInstVars: #()
			poolDictionaries: #()
			inDictionary: symDict2
			options: #().
	theClass 
		comment: classComment;
		category: (packageNames2 at: 1).
	theClass
		compileMethod: 'instanceBar ^true'
			dictionaries: symbolList
			category: #'accessing'
			intoMethodDict: nil
			intoCategories: nil
			environmentId: 0;
		compileMethod: 'instanceExtensionFoo ^true'
			dictionaries: symbolList
			category: ('*', (packageNames1 at: 2)) asSymbol
			intoMethodDict: nil
			intoCategories: nil
			environmentId: 0.
	theClass class
		compileMethod: 'classBar ^true'
			dictionaries: symbolList
			category: #'accessing'
			intoMethodDict: nil
			intoCategories: nil
			environmentId: 0;
		compileMethod: 'classExtensionFoo ^true'
			dictionaries: symbolList
			category: ('*', (packageNames1 at: 2)) asSymbol
			intoMethodDict: nil
			intoCategories: nil
			environmentId: 0.

"Boom"
	projectTools adopt adoptProjectSetDefinition: projectSetDefinition.

	"validate that adopted project structure matches the project defintions"
	loadedProjectSet := projectSetDefinition deriveLoadedThings
		asProjectDefinitionSet.
	diff := projectSetDefinition compareAgainstBase: loadedProjectSet.
	self assert: diff isEmpty.
]
