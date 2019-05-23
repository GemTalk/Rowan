tests-issue 313
testIssue313_6

	"https://github.com/dalehenrich/Rowan/issues/313"

	"test case for HR9880 - testing interactive api load and subclasses in different projects"

	| projectName1 projectName2  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition1 
		classDefinition packageDefinition className1 projectSetDefinition baseClass oldClass |

	projectName1 := 'HR9880_1'.
	projectName2 := 'HR9880_2'.
	packageName1 := 'HR9880_1-Core'.
	packageName2 := 'HR9880_2-Core'.
	className1 := 'HR9880'.
	{projectName1 . projectName2 }
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create projects"
	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName1)
		addPackageNamed: packageName1;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.
	projectDefinition2 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName2)
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	1 to: 10 do: [:index | 
		projectDefinition2 addPackageNamed: packageName2, '-', index printString ].


	packageDefinition := projectDefinition1 packageNamed: packageName1.

	classDefinition1 := RwClassDefinition
		newForClassNamed: className1 
			super: 'Object'
			instvars: #(iv1 iv2)
			classinstvars: #()
			classvars: #()
			category:packageName1
			comment: 'comment'
			pools: #()
			type: 'normal'.
	packageDefinition 
		addClassDefinition: classDefinition1.

	1 to: 10 do: [:index | 
		| thePackageName thePackageDefinition |
		thePackageName := packageName2, '-', index printString.
		thePackageDefinition := projectDefinition2 packageNamed: thePackageName.

		classDefinition := RwClassDefinition
			newForClassNamed: className1, '_', index printString
				super: className1
				instvars: #()
				classinstvars: #()
				classvars: #()
				category: thePackageName
				comment: 'comment'
				pools: #()
				type: 'normal'.
		thePackageDefinition 
			addClassDefinition: classDefinition.

		classDefinition
			addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'mover'
						protocol: 'accessing'
						source: 'foo ^1') ].

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition 
		addDefinition: projectDefinition1;
		addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"

	baseClass := Rowan globalNamed: className1.
	self assert: baseClass rowanPackageName = packageName1.
	1 to: 10 do: [:index | 
		| theClass thePackageName |

		theClass := Rowan globalNamed: className1 , '_', index printString.
		thePackageName := packageName2, '-', index printString.

		self assert: theClass superclass == baseClass.
		self assert: theClass rowanPackageName = thePackageName.
		self assert: (theClass compiledMethodAt: #foo) rowanPackageName = thePackageName ].

"modify class -- new version"

	TestCase rwSubclass:className1
	instVarNames: #( iv1 iv2)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	category: packageName1
	options: #().

"validate"
	oldClass := baseClass.
	baseClass := Rowan globalNamed: className1.
	self assert: baseClass rowanPackageName = packageName1.
	self assert: oldClass ~~ baseClass.
	1 to: 10 do: [:index | 
		| theClass thePackageName |

		theClass := Rowan globalNamed: className1 , '_', index printString.
		thePackageName := packageName2, '-', index printString.

		self assert: theClass superclass == baseClass.
		self assert: theClass rowanPackageName = thePackageName.
		self assert: (theClass compiledMethodAt: #foo) rowanPackageName = thePackageName ].
