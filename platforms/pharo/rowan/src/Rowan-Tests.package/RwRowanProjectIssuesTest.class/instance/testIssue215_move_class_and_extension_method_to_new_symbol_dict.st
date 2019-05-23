tests-issue 215
testIssue215_move_class_and_extension_method_to_new_symbol_dict

	"https://github.com/dalehenrich/Rowan/issues/215"

	| projectName  packageName1 packageName2 packageName3 projectDefinition1 projectDefinition2 classDefinition packageDefinition className1 className2 projectSetDefinition class
		classExtensionDefinition oldClass |
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

	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		setSymbolDictName: self _symbolDictionaryName1
			forPackageNamed: packageName1;
		setSymbolDictName: self _symbolDictionaryName1
			forPackageNamed: packageName2;
		yourself.

	packageDefinition := projectDefinition1 packageNamed: packageName1.
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

	packageDefinition := projectDefinition1 packageNamed: packageName2.
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
				protocol: 'accessing'
			source: 'mover ^2').
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	"validate"
	class := Rowan globalNamed: className1.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName2.
	self assert: class new mover = 2.

	"move the class to different symbol dictionary and move extension methods to new package"
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.
	projectDefinition2
		addPackageNamed: packageName3;
		setSymbolDictName: self _symbolDictionaryName2
			forPackageNamed: packageName2;
		setSymbolDictName: self _symbolDictionaryName1
			forPackageNamed: packageName3.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	(packageDefinition classExtensions at: className1) removeInstanceMethod: #mover.

	packageDefinition := projectDefinition2 packageNamed: packageName3.
	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'mover'
				protocol: 'accessing'
			source: 'mover ^2').
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.
	
	"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	"validate"
	oldClass := class.
	class := Rowan globalNamed: className1.
	self assert: class == oldClass.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName3.
	self assert: class new mover = 2.