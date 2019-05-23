tests - classes
testAddClassExtension

	| projectName packageNames packageName1 packageName2 className testClass testInstance |
	projectName := 'SimpleClassExtension'.
	packageName1 := 'Simple-Core'.
	packageName2 := 'Simple-Extension'.
	packageNames := {packageName1.
	packageName2}.
	className := 'SimpleEdit'.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	self
		_loadExtendedProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment:
			'This is a simple project used to illustrate how to edit an existing class.'
		className: className
		inPackageName: packageName1
		extensionPackageName: packageName2.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	self assert: testClass civar1 = 1.
	self assert: (testClass cadd: 1) = 2.
	self assert: testClass cvar1 = 2.
	testInstance := testClass new.
	testInstance ivar1: 2.
	self assert: testInstance ivar1 = 2.
	self assert: (testInstance add: 1) = 3
