tests
testIssue420_E

	"https://github.com/GemTalk/Rowan/issues/420"

	"move method from unpackaged category to category in another unpackaged category (class is unpackaged)"

	| projectName  packageName1 projectDefinition projectSetDefinition audit testClass symDict registry |

	projectName := 'Issue364'.
	packageName1 := 'Issue364-Extension-1'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	testClass := self _testClass.
	testClass addCategory: 'accessing'.
	[ testClass
		rwCompileMethod: 'bar ^''bar'''
		category: 'accessing' ]
			on: RwPerformingUnpackagedEditNotification
			do: [:ex | ex resume ].	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = 'accessing' asSymbol.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

"move"
	testClass addCategory: 'other accessing'.
	testClass rwMoveMethod: #bar toCategory: 'other accessing'.	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = #'other accessing'.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

"load -- effectively unload extension methods"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	[ testClass rwRemoveSelector: #bar ]
			on: RwPerformingUnpackagedEditNotification
			do: [:ex | ex resume ].	
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) isNil.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.