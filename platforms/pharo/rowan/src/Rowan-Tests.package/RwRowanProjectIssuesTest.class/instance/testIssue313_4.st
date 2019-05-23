tests-issue 313
testIssue313_4

	"https://github.com/dalehenrich/Rowan/issues/313"

	"test case for HR9880 - testing interactive api load and test methods as extension methods"

	| projectName  packageName1 packageName2 projectDefinition classDefinition1 classDefinition 
		packageDefinition1 packageDefinition2 className1 projectSetDefinition baseClass oldClass 
		classExtensionDefinition |

	projectName := 'HR9880'.
	packageName1 := 'HR9880-Core'.
	packageName2 := 'HR9880-Extension'.
	className1 := 'HR9880'.
	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	packageDefinition1 := projectDefinition packageNamed: packageName1.
	packageDefinition2 := projectDefinition packageNamed: packageName2.

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
	packageDefinition1
		addClassDefinition: classDefinition1.

	1 to: 10 do: [:index | 
		| className |

		className := className1, '_', index printString.

		classDefinition := RwClassDefinition
			newForClassNamed: className
				super: className1
				instvars: #()
				classinstvars: #()
				classvars: #()
				category: packageName1
				comment: 'comment'
				pools: #()
				type: 'normal'.
		packageDefinition1 addClassDefinition: classDefinition.

		classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className.
		classExtensionDefinition
			addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'mover'
						protocol: '*', packageName2 asLowercase
						source: 'foo ^1').
		packageDefinition2 addClassExtensionDefinition: classExtensionDefinition ].


"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"

	baseClass := Rowan globalNamed: className1.
	1 to: 10 do: [:index | 
		| theClass |

		theClass := Rowan globalNamed: className1 , '_', index printString.

		self assert: theClass superclass == baseClass.
		self assert: (theClass compiledMethodAt: #foo) rowanPackageName = packageName2 ].

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
	self assert: oldClass ~~ baseClass.
	1 to: 10 do: [:index | 
		| theClass |

		theClass := Rowan globalNamed: className1 , '_', index printString.

		self assert: theClass superclass == baseClass.
		self assert: (theClass compiledMethodAt: #foo) rowanPackageName = packageName2 ].