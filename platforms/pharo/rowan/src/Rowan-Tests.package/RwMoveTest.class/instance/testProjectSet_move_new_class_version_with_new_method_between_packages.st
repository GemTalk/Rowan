tests - issue 254
testProjectSet_move_new_class_version_with_new_method_between_packages

	"Use the regular load stack"

	"https://github.com/dalehenrich/Rowan/issues/316"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition 
		className projectSetDefinition class x newClass symDict registry |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Core2'.
	className := 'Issue254Class'.

"create project"
	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

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

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName1.

"create new version and add new method"
	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName1.
	packageDefinition classDefinitions removeKey: classDefinition key.

	classDefinition 
		instVarNames: (classDefinition instVarNames copy add: 'ivar1'; yourself). "force new class version"
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method2'
					protocol: 'accessing'
					source: 'method2 ^2').

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition addClassDefinition: classDefinition.

"load"
	projectSetDefinition addDefinition: projectDefinition2.

"validate"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	newClass := Rowan globalNamed: className.
	self assert: (x := newClass rowanPackageName) = packageName2.
	self assert: (newClass compiledMethodAt: #method1) rowanPackageName = packageName2.
	self assert: newClass ~~ class.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 2.
