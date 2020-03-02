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
		addComponentNamed: 'Core' 
			definedGroupNames: 
				(Dictionary new
						add: 'core' -> {};
						yourself) 
			comment: '';
		addPackageNamed: packageName1 
			toComponentNamed: 'Core' 
			withConditions: #('common') 
			andGroupName: #('core');
		addPackageNamed: packageName2 
			toComponentNamed: 'Core' 
			withConditions: #('common') 
			andGroupName: #('core');
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
	project2 := (Rowan image loadedProjectNamed: projectName) asDefinition.
	project2
		addPackageNamed: packageName3 
			toComponentNamed: 'Core' 
			withConditions: #('common') 
			andGroupName: #('core');
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
