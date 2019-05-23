tests - issue 254
testProjectSet_move_class_with_extension_method_between_packages_and_symbolDict

	"https://github.com/dalehenrich/Rowan/issues/316"

	"initial load uses symbol dictionary _1, second load uses symbol dictionary _2"

	| projectName  packageName1 packageName2 packageName3 packageName4 projectDefinition1 projectDefinition2 
		classDefinition packageDefinition className projectSetDefinition class classExtensionDefinition
		ar oldClass loadedClass symDict registry |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Extension2'.
	packageName3 := 'Issue254-Core3'.
	packageName4 := 'Issue254-Extension4'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"setup"
	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
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

	classExtensionDefinition := RwClassExtensionDefinition
		newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #ext1
						protocol: 'extension'
						source: 'ext1 ^ 1').

	packageDefinition := projectDefinition1 packageNamed: packageName2.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #ext1) rowanPackageName = packageName2.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName1 asSymbol.

	projectSetDefinition := RwProjectSetDefinition new.

"move class and extension method to different packages and symbol dictionaries"
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition
		removePackageNamed: packageName1;
		removePackageNamed: packageName2;
		addPackageNamed: packageName3;
		addPackageNamed: packageName4;
		setSymbolDictName: self _symbolDictionaryName2 forPackageNamed: packageName3;
		setSymbolDictName: self _symbolDictionaryName2 forPackageNamed: packageName4.

	packageDefinition := projectDefinition2 packageNamed: packageName3.
	packageDefinition addClassDefinition: classDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName4.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	projectSetDefinition addDefinition: projectDefinition2.
"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	loadedClass := Rowan image loadedClassNamed: className.
	loadedClass loadedInstanceMethods includesKey: #method1.

	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName3.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName3.
	self assert: (class compiledMethodAt: #ext1) rowanPackageName = packageName4.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2 asSymbol.

	self assert: class == oldClass.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 0.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 2.