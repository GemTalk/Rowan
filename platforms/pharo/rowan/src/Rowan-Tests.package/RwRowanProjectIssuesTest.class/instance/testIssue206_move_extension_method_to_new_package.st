tests-issue 206
testIssue206_move_extension_method_to_new_package

	"https://github.com/dalehenrich/Rowan/issues/206"

	"based on testIssue185_move_extension_method_to_new_package_1"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition className projectSetDefinition class
		classExtensionDefinition packageName3 |
	projectName := 'Issue185'.
	packageName1 := 'Issue185-Core1'.
	packageName2 := 'Issue185-Core2'.
	packageName3 := 'Issue185-Core3'.
	className := 'Issue185Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		addPackageNamed: packageName3;
		defaultSymbolDictName: self _symbolDictionaryName1;
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

	"create extension method in different package"
	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'mover'
					protocol: '*', packageName2 asLowercase
					source: 'mover ^2').

	packageDefinition := projectDefinition1 packageNamed: packageName2.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	"validate"
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: class new mover = 2.

	"move the method to new package"
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition classExtensions removeKey: className.										"this is the source of the bug ... as a new class extensions without the proper class name is created
																															during the comparison"

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'mover'
					protocol: '*', packageName3 asLowercase
					source: 'mover ^2').

	packageDefinition := projectDefinition2 packageNamed: packageName3.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	"validate"
	self assert: class new mover = 2. "method not installed when class extension does not have a proper class name"