tests-issue 185
testIssue185_254_rename_package_move_class_to_symbolDict_3

	"https://github.com/dalehenrich/Rowan/issues/185"

	"initial load uses custom symbol dictionary, second load uses default symbol dictionary -- change category of class"

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

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2 asSymbol.

	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition
		removePackageNamed: packageName1;
		addPackageNamed: packageName2;
		setSymbolDictName: nil forPackageNamed: packageName2.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	classDefinition category: packageName2.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName2.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (x := (ar first at: 1) name) = (y := self _symbolDictionaryName1 asSymbol).

	self assert: class == oldClass.
