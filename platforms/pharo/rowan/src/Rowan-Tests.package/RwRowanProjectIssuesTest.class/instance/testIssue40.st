tests-issue 40
testIssue40

	"https://github.com/dalehenrich/Rowan/issues/40"

	| projectName packageName theClass1 theClass2 projectDefinition classDefinition |
	projectName := 'Issue40_project'.
	packageName := 'Issue40-Core'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject/' validate: false.
	self _addPackageNamed: packageName toProjectNamed: projectName validate: false.

	theClass1 := self 
		_addClassNamed: 'Issue40Class_1' 
		superclassName: 'Object' 
		classVars: #( CVar1 )
		toPackageNamed: packageName 
		inProjectNamed: projectName 
		validate: false.

	theClass2 := self 
		_addClassNamed: 'Issue40Class_2' 
		superclassName: 'Issue40Class_1' 
		classVars: #( CVar2 )
		toPackageNamed: packageName 
		inProjectNamed: projectName 
		validate: false.

	self _addOrUpdateMethod: 'cvar1 ^CVar1' forBehavior: theClass2 class inPackage: packageName inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod: 'cvar2 ^CVar2' forBehavior: theClass2 class inPackage: packageName inProjectNamed: projectName validate: false.

	"add both class initializationMethods at once, so that order will be important - probably random order, unfortunately"
	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName) classDefinitions at:  'Issue40Class_1'.
	classDefinition addClassMethodDefinition: (RwMethodDefinition
						newForSelector: #'initialize'
						protocol: 'initialization'
						source: 'initialize CVar1 := 1.').
	classDefinition := (projectDefinition packageNamed: packageName) classDefinitions at:  'Issue40Class_2'.
	classDefinition addClassMethodDefinition: (RwMethodDefinition
						newForSelector: #'initialize'
						protocol: 'initialization'
						source: 'initialize CVar2 := CVar1 + 1.').
	[ Rowan projectTools load loadProjectDefinition: projectDefinition ]
			on: RwExecuteClassInitializeMethodsAfterLoadNotification
			do: [:ex | ex resume: true ].

	self assert: theClass2 cvar1 = 1.
	self assert: theClass2 cvar2 = 2.
