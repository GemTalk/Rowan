tests - issue 254
testProjectSet_move_method_between_packages

	"Use the regular load stack"

	"https://github.com/dalehenrich/Rowan/issues/254"

	| projectName  packageName1 packageName2 packageName3 projectDefinition1 projectDefinition2 classDefinition packageDefinition 
		className projectSetDefinition class symDict registry classExtensionDefinition oldClass loadedPackage loadedClass loadedClassExtensions |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Extension2'.
	packageName3 := 'Issue254-Extension3'.
	className := 'Issue254Class'.

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

	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'mover'
					protocol: 'accessing'
					source: 'mover ^1').

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	class := Rowan globalNamed: className.
	self assert: class new mover = 1.
	self assert: class rowanPackageName = packageName1.
	self assert: (class categoryOfSelector: #mover) asString = 'accessing'.
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName1.

	loadedPackage := Rowan image loadedPackageNamed: packageName1.
	loadedClass := loadedPackage loadedClasses at: className.
	self assert: (loadedClass loadedInstanceMethods includesKey: #mover).
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: loadedClassExtensions isEmpty.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 3.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 1.

	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName1.
	classDefinition := packageDefinition classDefinitions at: classDefinition key.
	classDefinition removeInstanceMethod: #mover.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'mover'
					protocol: '*', packageName2 asLowercase
					source: 'mover ^1').

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	projectSetDefinition addDefinition: projectDefinition2.

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class == oldClass.
	self assert: class new mover = 1.
	self assert: class rowanPackageName = packageName1.
	self assert: (class categoryOfSelector: #mover) asString =  ('*', packageName2 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName2.

	loadedPackage := Rowan image loadedPackageNamed: packageName1.
	loadedClass := loadedPackage loadedClasses at: className.
	self assert: loadedClass instanceMethodDefinitions isEmpty.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: loadedClassExtensions isEmpty.

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClass := loadedPackage loadedClasses at: className ifAbsent: [].
	self assert: loadedClass isNil.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) loadedInstanceMethods includesKey: #mover).

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 3.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 1.