tests-issue 185
testIssue185_rename_package_move_class_to_symbolDict_add_remove_and_remove_methods_1

	"https://github.com/dalehenrich/Rowan/issues/185"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition className projectSetDefinition class
		ar oldClass x y |
	projectName := 'Issue185'.
	packageName1 := 'Issue185-Core'.
	packageName2 := 'Issue185-RenamedCore'.
	className := 'Issue185Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		defaultSymbolDictName: self _symbolDictionaryName1;
		setSymbolDictName: self _symbolDictionaryName2 forPackageNamed: packageName1;
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
					source: 'method1 ^1');
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method2'
					protocol: 'accessing'
					source: 'method2 ^2');
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method3'
					protocol: 'accessing'
					source: 'method3 ^3').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2 asSymbol.
	self assert: class new method1 = 1.
	self assert: class new method2 = 2.

	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition
		removePackageNamed: packageName1;
		addPackageNamed: packageName2;
		setSymbolDictName: nil forPackageNamed: packageName2.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	classDefinition category: packageName2.
	packageDefinition addClassDefinition: classDefinition.

	classDefinition
		removeInstanceMethod: #method2;
		removeInstanceMethod: #method3;
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method3'
					protocol: 'accessing'
					source: 'method3 ^5');
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method4'
					protocol: 'accessing'
					source: 'method4 ^4').

	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName2.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (x := (ar first at: 1) name) = (y := self _symbolDictionaryName1 asSymbol).
	self assert: class new method1 = 1.
	self should: [ class new method2 = 2 ] raise: MessageNotUnderstood.
	self assert: (x := class new method3) = 5.
	self assert: class new method4 = 4.

	self assert: class == oldClass.
