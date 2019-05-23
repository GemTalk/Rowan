tests - classes
testUpdateClass

	| projectName packageNames packageName className projectTools projectDefinition classDefinition testClass testInstance |
	projectName := 'Simple'.
	packageName := 'Simple-Core'.
	packageNames := {packageName}.
	className := 'SimpleEdit'.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	self
		_loadStandardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment:
			'This is a simple project used to illustrate how to edit an existing class.'
		className: className
		inPackageName: packageName.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	self assert: testClass civar1 = 1.
	self assert: testClass cvar1 = 2.
	testInstance := testClass new.
	self assert: testInstance ivar1 isNil.

	projectTools := Rowan projectTools.
	projectTools edit
		definitionsForClassNamed: className
		ifFound: [ :classDef :packageDef :projectDef | 
			projectDefinition := projectDef.
			classDefinition := classDef ]
		ifAbsent: [ 
			self
				assert: false
				decription:
					'expected class definition for class ' , className printString , ' not found.' ].

	classDefinition
		instVarNames: #();
		removeInstanceMethod: #'ivar1';
		classVarNames: #();
		removeClassMethod: #'cvar1';
		updateClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'initialize'
						protocol: 'initialization'
						source: 'initialize civar1 := 1.');
		yourself.

	projectTools edit
		updateOrAddClass: classDefinition
		inPackageNamed: packageName
		inProject: projectDefinition.

	projectTools load loadProjectDefinition: projectDefinition.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	self should: [ testClass cvar1 ] raise: MessageNotUnderstood.
	self assert: testClass civar1 = 1.
	testInstance := testClass new.
	self should: [ testInstance ivar1 ] raise: MessageNotUnderstood
