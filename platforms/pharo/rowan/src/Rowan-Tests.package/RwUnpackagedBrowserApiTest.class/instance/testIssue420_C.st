tests
testIssue420_C

	"https://github.com/GemTalk/Rowan/issues/420"

	"move method from packaged category to unpackaged category (class is unpackaged)"

	| projectName  packageName1 packageName2 projectDefinition projectSetDefinition audit testClass symDict registry |

	projectName := 'Issue364'.
	packageName1 := 'Issue364-Extension-1'.
	packageName2 := 'Issue364-Extension-2'.

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

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	testClass := self _testClass.
	testClass addCategory: '*' , packageName1 asLowercase.
	testClass
		rwCompileMethod: 'bar ^''bar'''
		category: '*' , packageName1 asLowercase.

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = ('*' , packageName1 asLowercase) asSymbol.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 1.

"move"
	testClass addCategory: 'accessing'.
	[ testClass rwMoveMethod: #bar toCategory:  'accessing' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = 'accessing'  asSymbol.
"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

"load -- effectively unload extension methods"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	[ testClass rwRemoveSelector: #bar ]
			on: RwPerformingUnpackagedEditNotification
			do: [:ex | ex resume ].	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) isNil.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.