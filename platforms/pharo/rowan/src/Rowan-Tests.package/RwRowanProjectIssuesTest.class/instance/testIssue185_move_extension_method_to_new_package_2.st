tests-issue 185
testIssue185_move_extension_method_to_new_package_2

	"https://github.com/dalehenrich/Rowan/issues/185"

	"simplified version of RwRowanProjectIssuesTest>>testIssue41_interactiveMoveInitializeExtensionMethodToPackage"

	"use Behavior>>rwCompileMethod:category: to do final update (testIssue185_move_extension_method_to_new_package_1 exposes a bug)"

	| projectName  packageName1 packageName2 projectDefinition1 classDefinition packageDefinition className projectSetDefinition class
		classExtensionDefinition packageName3 loadedPackage loadedClassExtensions x |
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
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

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
	self assert: class new method1 = 1.
	self assert: class new mover = 2.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName2 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName2.

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: (loadedClassExtensions at: className ifAbsent: []) isNil.

	"move the method to new package"
	class 
		rwCompileMethod: 'mover ^2'
		category:  '*' , packageName3 asLowercase.

	"validate"
	self assert: class rowanPackageName = packageName1.
	self assert: class new method1 = 1.
	self assert: class new mover = 2.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName3 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName3.

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self deny: ((x := loadedClassExtensions at: className ifAbsent: [RwGsLoadedSymbolDictClassExtension new initialize]) instanceMethodDefinitions includesKey: #mover).