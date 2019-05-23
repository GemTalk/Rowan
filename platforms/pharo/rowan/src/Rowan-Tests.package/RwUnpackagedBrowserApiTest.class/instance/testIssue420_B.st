tests
testIssue420_B

	"https://github.com/GemTalk/Rowan/issues/420"

	"move method from extension category to packaged category (class is packaged)"

	| projectName  packageName1 packageName2 projectDefinition projectSetDefinition audit testClass symDict registry |

	projectName := 'Issue364'.
	packageName1 := 'Issue364-Core-1'.
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

	testClass := Object rwSubclass: self _testPackagedClassName
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	testClass addCategory: '*' , packageName2 asLowercase.
	testClass
		rwCompileMethod: 'bar ^''bar'''
		category: '*' , packageName2 asLowercase.

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = ('*' , packageName2 asLowercase) asSymbol.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 1.

"move"
	testClass addCategory: 'accessing'.
	testClass rwMoveMethod: #bar toCategory: 'accessing'.

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = #'accessing'.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 1.

"load -- effectively unload extension methods"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	self assert: (Rowan globalNamed: self _testPackagedClassName) isNil.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.

"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.