Extension { #name : 'RwRowanProjectIssuesTest' }

{ #category : '*rowan-tests-gemstonev1' }
RwRowanProjectIssuesTest >> testIssue165 [

	"https://github.com/dalehenrich/Rowan/issues/165"

	| projectName packageName1 className symDictName theClass theSymbolDict myUserProfile mySymbolList theSymDictIndex | 
	projectName := 'Issue165_Project'.
	packageName1 := 'Issue165-Core'.
	className := 'Issue165Class'.
	symDictName := self _symbolDictionaryName2.

	self 
		_createLoadedProjectNamed: projectName 
		root: self _testRowanProjectsSandbox pathString, '/rowanIssuesProject/' 
		symbolDictionaryName: symDictName 
		validate: false.

	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.

	theClass := Object
		rwSubclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	theClass rwCompileMethod: 'foo ^1' category: 'accessing'.

	theSymbolDict := Rowan globalNamed: symDictName.
	self assert: theSymbolDict class == SymbolDictionary.			"symbol dictionary is accessible in my symbol list"

	self assert: (Rowan projectNames includes: projectName).		"project is visible"
	self assert: (Rowan packageNames includes: packageName1).	"package is visible"
	self assert: (Rowan globalNamed: className) notNil.				"class is visible"

	myUserProfile := System myUserProfile.
	mySymbolList := System myUserProfile symbolList.
	theSymDictIndex := mySymbolList indexOf: theSymbolDict.
	myUserProfile removeDictionaryAt: theSymDictIndex.				"remove symbol dictionary from my symbol list"

	self assert: (Rowan projectNames includes: projectName).		"project is visible"
	self deny: (Rowan packageNames includes: packageName1).	"package is NOT visible"
	self deny: (Rowan globalNamed: className) notNil.				"class is NOT visible"
]

{ #category : '*rowan-tests-gemstonev1' }
RwRowanProjectIssuesTest >> testIssue185_254_move_extension_method_to_new_package_1 [

	"https://github.com/dalehenrich/Rowan/issues/185"
	"https://github.com/dalehenrich/Rowan/issues/254"

	"simplified version of RwRowanProjectIssuesTest>>testIssue41_interactiveMoveInitializeExtensionMethodToPackage"

	"use RwClassExtensionDefinition>>removeKey: to do final update (source changed)--- definition comparison bug (see testIssue206_move_extension_method_to_new_package_1)"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition 
		packageDefinition className projectSetDefinition class classExtensionDefinition packageName3 
		loadedPackage loadedClassExtensions x audit project|
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
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

	"validate"
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class new perform: #method1) = 1.
	self assert: (class new perform: #mover) = 2.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName2 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName2.

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: (loadedClassExtensions at: className ifAbsent: []) isNil.

	"move the method to new package"
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition classExtensions removeKey: className.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'mover'
					protocol: '*', packageName3 asLowercase
					source: 'mover ^3').

	packageDefinition := projectDefinition2 packageNamed: packageName3.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

	"validate"
	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

	self assert: class rowanPackageName = packageName1.
	self assert: (class new perform: #method1) = 1.
	self assert: (class new perform: #mover) = 3.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName3 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName3.

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self deny: ((x := loadedClassExtensions at: className ifAbsent: [RwGsLoadedSymbolDictClassExtension new initialize]) instanceMethodDefinitions includesKey: #mover).
]

{ #category : '*rowan-tests-gemstonev1' }
RwRowanProjectIssuesTest >> testIssue185_254_move_extension_method_to_new_package_3 [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"simplified version of RwRowanProjectIssuesTest>>testIssue41_interactiveMoveInitializeExtensionMethodToPackage"

	"use RwClassExtensionDefinition>>removeInstanceMethod: to do final update (testIssue185_move_extension_method_to_new_package_1 exposes a bug)"


	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition className projectSetDefinition class
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
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

	"validate"
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class new perform: #method1) = 1.
	self assert: (class new perform: #mover) = 2.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName2 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName2.

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: (loadedClassExtensions at: className ifAbsent: []) isNil.

	"move the method to new package"
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	(packageDefinition classExtensions at: className) removeInstanceMethod: #mover.

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
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

	"validate"
	self assert: class rowanPackageName = packageName1.
	self assert: (class new perform: #method1) = 1.
	self assert: (class new perform: #mover) = 2.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName3 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName3.

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self deny: ((x := loadedClassExtensions at: className ifAbsent: [RwGsLoadedSymbolDictClassExtension new initialize]) instanceMethodDefinitions includesKey: #mover).
]

{ #category : '*rowan-tests-gemstonev1' }
RwRowanProjectIssuesTest >> testIssue185_254_move_extension_method_to_new_package_4 [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"simplified version of RwRowanProjectIssuesTest>>testIssue41_interactiveMoveInitializeExtensionMethodToPackage"

	"use RwClassExtensionDefinition>>removeKey: to do final update (source not changed) --- definition comparison bug?"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition className projectSetDefinition class
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
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

	"validate"
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class new perform: #method1) = 1.
	self assert: (class new perform: #mover) = 2.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName2 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName2.

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: (loadedClassExtensions at: className ifAbsent: []) isNil.

	"move the method to new package"
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition classExtensions removeKey: className.

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
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

	"validate"
	self assert: class rowanPackageName = packageName1.
	self assert: (class new perform: #method1) = 1.
	self assert: (class new perform: #mover) = 2.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName3 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName3.

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self deny: ((x := loadedClassExtensions at: className ifAbsent: [RwGsLoadedSymbolDictClassExtension new initialize]) instanceMethodDefinitions includesKey: #mover).
]

{ #category : '*rowan-tests-gemstonev1' }
RwRowanProjectIssuesTest >> testIssue185_move_extension_method_to_new_package_1 [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"simplified version of RwRowanProjectIssuesTest>>testIssue41_interactiveMoveInitializeExtensionMethodToPackage"

	"use RwClassExtensionDefinition>>removeKey: to do final update (source changed)--- definition comparison bug (see testIssue206_move_extension_method_to_new_package_1)"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition className projectSetDefinition class
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
	self assert: (class new perform: #method1) = 1.
	self assert: (class new perform: #mover) = 2.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName2 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName2.

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: (loadedClassExtensions at: className ifAbsent: []) isNil.

	"move the method to new package"
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition classExtensions removeKey: className.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'mover'
					protocol: '*', packageName3 asLowercase
					source: 'mover ^3').

	packageDefinition := projectDefinition2 packageNamed: packageName3.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	"validate"
	self assert: class rowanPackageName = packageName1.
	self assert: (class new perform: #method1) = 1.
	self assert: (class new perform: #mover) = 3.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName3 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName3.

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self deny: ((x := loadedClassExtensions at: className ifAbsent: [RwGsLoadedSymbolDictClassExtension new initialize]) instanceMethodDefinitions includesKey: #mover).
]

{ #category : '*rowan-tests-gemstonev1' }
RwRowanProjectIssuesTest >> testIssue185_move_extension_method_to_new_package_2 [

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
	self assert: (class new perform: #method1) = 1.
	self assert: (class new perform: #mover) = 2.

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
	self assert: (class new perform: #method1) = 1.
	self assert: (class new perform: #mover) = 2.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName3 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName3.

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self deny: ((x := loadedClassExtensions at: className ifAbsent: [RwGsLoadedSymbolDictClassExtension new initialize]) instanceMethodDefinitions includesKey: #mover).
]

{ #category : '*rowan-tests-gemstonev1' }
RwRowanProjectIssuesTest >> testIssue185_move_extension_method_to_new_package_3 [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"simplified version of RwRowanProjectIssuesTest>>testIssue41_interactiveMoveInitializeExtensionMethodToPackage"

	"use RwClassExtensionDefinition>>removeInstanceMethod: to do final update (testIssue185_move_extension_method_to_new_package_1 exposes a bug)"


	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition className projectSetDefinition class
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
	self assert: (class new perform: #method1) = 1.
	self assert: (class new perform: #mover) = 2.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName2 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName2.

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: (loadedClassExtensions at: className ifAbsent: []) isNil.

	"move the method to new package"
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	(packageDefinition classExtensions at: className) removeInstanceMethod: #mover.

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
	self assert: class rowanPackageName = packageName1.
	self assert: (class new perform: #method1) = 1.
	self assert: (class new perform: #mover) = 2.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName3 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName3.

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self deny: ((x := loadedClassExtensions at: className ifAbsent: [RwGsLoadedSymbolDictClassExtension new initialize]) instanceMethodDefinitions includesKey: #mover).
]

{ #category : '*rowan-tests-gemstonev1' }
RwRowanProjectIssuesTest >> testIssue185_move_extension_method_to_new_package_4 [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"simplified version of RwRowanProjectIssuesTest>>testIssue41_interactiveMoveInitializeExtensionMethodToPackage"

	"use RwClassExtensionDefinition>>removeKey: to do final update (source not changed) --- definition comparison bug?"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition className projectSetDefinition class
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
	self assert: (class new perform: #method1) = 1.
	self assert: (class new perform: #mover) = 2.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName2 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName2.

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: (loadedClassExtensions at: className ifAbsent: []) isNil.

	"move the method to new package"
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition classExtensions removeKey: className.

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
	self assert: class rowanPackageName = packageName1.
	self assert: (class new perform: #method1) = 1.
	self assert: (class new perform: #mover) = 2.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName3 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName3.

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self deny: ((x := loadedClassExtensions at: className ifAbsent: [RwGsLoadedSymbolDictClassExtension new initialize]) instanceMethodDefinitions includesKey: #mover).
]
