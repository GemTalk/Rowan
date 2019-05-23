tests
testIssue263

	"https://github.com/GemTalk/Rowan/issues/263"

	"Add extension method to an unpackaged class"

	| projectName  packageName projectDefinition projectSetDefinition audit testClass |

	projectName := 'Issue263_A'.
	packageName := 'Issue263_A-Extension'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	self assert: (projectDefinition projectDefinitionSourceProperty = RwLoadedProject _projectUnknownDefinitionSourceValue).

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	testClass := self _testClass.
	testClass
		rwCompileMethod: 'bar ^''bar'''
		category: '*' , packageName asLowercase.

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.

"load -- effectively unload extension methods"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) isNil.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.