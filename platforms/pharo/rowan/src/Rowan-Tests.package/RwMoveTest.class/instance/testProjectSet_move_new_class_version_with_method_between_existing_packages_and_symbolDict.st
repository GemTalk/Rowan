tests - issue 254
testProjectSet_move_new_class_version_with_method_between_existing_packages_and_symbolDict
	"Use the regular load stack"

	"https://github.com/dalehenrich/Rowan/issues/316"

	"initial load uses symbol dictionary _1, second load uses symbol dictionary _2"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition 
		className projectSetDefinition class oldClass ar loadedClass symDict registry |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Core2'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: nil
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

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	loadedClass := Rowan image loadedClassNamed: className.
	loadedClass loadedInstanceMethods includesKey: #method1.

	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName1 asSymbol.

	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition
		addPackageNamed: packageName2;
		setSymbolDictName: self _symbolDictionaryName2 forPackageNamed: packageName2.

	classDefinition 
		instVarNames: (classDefinition instVarNames copy add: 'ivar1'; yourself). "force new class version"

	packageDefinition := projectDefinition2 packageNamed: packageName1.
	packageDefinition removeClassNamed: classDefinition name.
	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	loadedClass := Rowan image loadedClassNamed: className.
	loadedClass loadedInstanceMethods includesKey: #method1.

	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName2.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName2.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2 asSymbol.

	self assert: class ~~ oldClass.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 1.
