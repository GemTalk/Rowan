tests - issue 254
testProjectSet_move_new_class_version_with_extension_method_between_existing_packages

	"https://github.com/dalehenrich/Rowan/issues/316"

	| projectName  packageName1 packageName2 packageName3 packageName4 projectDefinition1 projectDefinition2 
		classDefinition packageDefinition className projectSetDefinition class x newClass symDict registry 
		classExtensionDefinition |
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
		addPackageNamed: packageName3;
		addPackageNamed: packageName4;
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

"move class with new version and extension method to new packages"
	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	classDefinition 
		instVarNames: (classDefinition instVarNames copy add: 'ivar1'; yourself). "force new class version"

	packageDefinition := projectDefinition2 packageNamed: packageName1.
	packageDefinition removeClassDefinition: classDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition removeClassExtensionDefinition: classExtensionDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName3.
	packageDefinition addClassDefinition: classDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName4.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	projectSetDefinition addDefinition: projectDefinition2.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	newClass := Rowan globalNamed: className.
	self assert: (x := newClass rowanPackageName) = packageName3.
	self assert: (newClass compiledMethodAt: #method1) rowanPackageName = packageName3.
	self assert: (x := (newClass compiledMethodAt: #ext1) rowanPackageName) = packageName4.
	self assert: newClass ~~ class.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 4.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 2.