tests - classes
testAddAndRemoveClass

	| projectName projectDefinition projectTools packageNames classDefinition packageName testClass testInstance className |
	projectName := 'Simple'.
	packageName := 'Simple-Core'.
	packageNames := {packageName}.
	projectTools := Rowan projectTools.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	projectDefinition := self
		_standardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment:
			'This is a simple project used to work out a "classic" class browser-style API'.

	className := 'SimpleEdit'.
	classDefinition := self _standardClassDefinition: className.

	projectTools edit
		addClass: classDefinition
		inPackageNamed: packageName
		inProject: projectDefinition.

	[ projectTools load loadProjectDefinition: projectDefinition ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | ex resume: true ].

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	self assert: testClass civar1 = 1.
	self assert: testClass cvar1 = 2.
	testInstance := testClass new.
	self assert: testInstance ivar1 isNil.


	projectTools edit
		removeClassNamed: className
		fromPackageNamed: packageName
		inProject: projectDefinition.

	[ projectTools load loadProjectDefinition: projectDefinition ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | ex resume: true ].

	testClass := Rowan globalNamed: className.
	self assert: testClass isNil
