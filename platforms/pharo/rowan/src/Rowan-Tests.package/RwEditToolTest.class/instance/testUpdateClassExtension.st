tests - classes
testUpdateClassExtension

	| projectName packageNames packageName1 packageName2 packageName3 className projectTools testClass testInstance classExtensionDefinitions |
	projectName := 'SimpleClassExtension'.
	packageName1 := 'Simple-Core'.
	packageName2 := 'Simple-Extension1'.
	packageName3 := 'Simple-Extension2'.
	packageNames := {packageName1.
	packageName2.
	packageName3}.
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
		extensionPackageNames:
			{packageName2.
			packageName3}.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	self assert: testClass civar1 = 1.
	self assert: (testClass cadd: 1) = 2.
	self assert: testClass cvar1 = 2.
	testInstance := testClass new.
	testInstance ivar1: 2.
	self assert: testInstance ivar1 = 2.
	self assert: (testInstance add: 1) = 3.

	classExtensionDefinitions := {}.
	projectTools := Rowan projectTools.
	projectTools edit
		definitionsForClassExtensionNamed: className
		ifFound: [ :classExtensionDef :packageDef :projectDef | 
			classExtensionDefinitions
				add:
					{classExtensionDef.
					packageDef.
					projectDef} ]
		ifAbsent: [ 
			self
				assert: false
				decription:
					'expected class extension definitions for class ' , className printString
						, ' not found.' ].
	self
		assert: classExtensionDefinitions size = 2
		description:
			'two packages contain extensions for this class ... classExtensionRegistry does not record multiple extensions correctly'.

	classExtensionDefinitions
		do: [ :ar | 
			| classExtensionDef packageDef projectDef |
			classExtensionDef := ar at: 1.
			packageDef := ar at: 2.
			projectDef := ar at: 3.
			self assert: projectDef name = projectName.
			self assert: (packageNames includes: packageDef name).
			packageDef name = packageName2
				ifTrue: [ 
					self
						assert: classExtensionDef instanceMethodDefinitions size = 1;
						assert:
								(classExtensionDef instanceMethodDefinitions includesKey: #'ivar1:');
						assert: classExtensionDef classMethodDefinitions size = 1;
						assert: (classExtensionDef classMethodDefinitions includesKey: #'cadd:') ].
			packageDef name = packageName3
				ifTrue: [ 
					self
						assert: classExtensionDef instanceMethodDefinitions size = 1;
						assert: (classExtensionDef instanceMethodDefinitions includesKey: #'add:') ] ]
