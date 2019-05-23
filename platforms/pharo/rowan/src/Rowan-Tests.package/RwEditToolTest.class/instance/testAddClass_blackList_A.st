tests - classes
testAddClass_blackList_A

	"https://github.com/GemTalk/Rowan/issues/447"

	"test that blackList is used to block class initialization - sessionAutomaticClassInitializationBlackList"

	| projectName projectDefinition projectTools packageNames classDefinition packageName testClass testInstance className |
	projectName := 'Simple'.
	packageName := 'Simple-Core'.
	packageNames := {packageName}.
	projectTools := Rowan projectTools.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

"create project definition"
	projectDefinition := self
		_standardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment:
			'Testing class initialization blackList'.

	className := 'SimpleEdit'.
	classDefinition := self _standardClassDefinition: className.

	projectTools edit
		addClass: classDefinition
		inPackageNamed: packageName
		inProject: projectDefinition.

"add project to session blackList"
	Rowan sessionAutomaticClassInitializationBlackList add: projectName.

"load project - initialization should not be triggered"
	[ projectTools load loadProjectDefinition: projectDefinition ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | self assert: false description: 'unexpected signal of RwExecuteClassInitializeMethodsAfterLoadNotification'  ].

"validate"
	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	self assert: testClass civar1 isNil.
	self assert: testClass cvar1 isNil.
	testInstance := testClass new.
	self assert: testInstance ivar1 isNil.