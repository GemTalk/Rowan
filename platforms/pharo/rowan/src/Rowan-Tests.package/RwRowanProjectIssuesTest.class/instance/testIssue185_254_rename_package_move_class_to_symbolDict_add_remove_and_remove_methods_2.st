tests-issue 185
testIssue185_254_rename_package_move_class_to_symbolDict_add_remove_and_remove_methods_2

	"https://github.com/dalehenrich/Rowan/issues/185"

	"minimal version of #testIssue185_rename_package_move_class_to_symbolDict_add_remove_and_remove_methods_1 isolating a particular failure
		that I'm trying to resolve"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition 
		className projectSetDefinition class x |
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
					newForSelector: #'method3'
					protocol: 'accessing'
					source: 'method3 ^3').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition
		removePackageNamed: packageName1;
		addPackageNamed: packageName2;
		setSymbolDictName: nil forPackageNamed: packageName2.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	classDefinition category: packageName2.
	packageDefinition addClassDefinition: classDefinition.

	classDefinition
		removeInstanceMethod: #method3;
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method3'
					protocol: 'accessing'
					source: 'method3 ^5').

	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

	class := Rowan globalNamed: className.

	self assert: (x := class new method3) = 5.
