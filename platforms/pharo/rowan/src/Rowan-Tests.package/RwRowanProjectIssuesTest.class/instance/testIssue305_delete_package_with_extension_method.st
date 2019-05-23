tests-issue 305
testIssue305_delete_package_with_extension_method

	"https://github.com/dalehenrich/Rowan/issues/305"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition 
		classExtension packageDefinition className projectSetDefinition class oldClass |
	projectName := 'Issue305'.
	packageName1 := 'Issue305-Core1'.
	packageName2 := 'Issue305-Core2'.
	className := 'Issue305Class'.

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

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	classExtension := (RwClassExtensionDefinition newForClassNamed: className)
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'foo'
						protocol: '*', packageName2 asLowercase
						source: 'foo ^ ''foo''');
		addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'foo'
						protocol: '*', packageName2 asLowercase
						source: 'foo ^ ''foo''');
		yourself.

	packageDefinition := projectDefinition1 packageNamed: packageName2.
	packageDefinition addClassExtensionDefinition: classExtension.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: class foo = 'foo'.
	self assert: class new foo = 'foo'.

"remove package"
	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	projectDefinition2 removePackageNamed: packageName2.

"load"
	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

"validate"
	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class == oldClass.
	self should: [ class foo = 'foo' ] raise: MessageNotUnderstood.
	self should: [ class new foo = 'foo' ] raise: MessageNotUnderstood.