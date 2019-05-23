tests-issue 114
testIssue114_methodSourceIsNotTheSame_1

	"https://github.com/dalehenrich/Rowan/issues/114"

	"existing method and comment in subclass, new class version of superclass, new method, update old method,
		change method protocol and update comment in subclass -- combo"

	| projectName packageName1 packageName2 theClass1 theClass2 projectDefinition classDefinition |
	projectName := 'Issue114_project'.
	packageName1 := 'Issue114-Core1'.
	packageName2 := 'Issue114-Core2'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject/' validate: false.
	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	theClass1 := self 
		_addClassNamed: 'Issue114Class_1' 
		superclassName: 'Object' 
		classVars: #(  )
		toPackageNamed: packageName1 
		inProjectNamed: projectName 
		validate: false.

	theClass2 := self 
		_addClassNamed: 'Issue114Class_2' 
		superclassName: 'Issue114Class_1' 
		classVars: #(  )
		toPackageNamed: packageName2
		inProjectNamed: projectName
		validate: false.
	theClass2 rwComment: 'a comment'.
	self _addOrUpdateMethod: 'foo ^1' forBehavior: theClass1  inPackage: packageName1 inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod: 'foo ^1' forBehavior: theClass2  inPackage: packageName2 inProjectNamed: projectName validate: false.

	self assert: theClass1 new foo = 1.
	self assert: theClass2 new foo = 1.
	self assert: theClass2 comment = 'a comment'.

	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName1) classDefinitions at:  'Issue114Class_1'.
	classDefinition instVarNames: #(ivar1).	"new class version"
	classDefinition := (projectDefinition packageNamed: packageName2) classDefinitions at:  'Issue114Class_2'.
	classDefinition 
		addInstanceMethodDefinition:  (RwMethodDefinition
						newForSelector: #'ivar1'
						protocol: 'accessing'
						source: 'ivar1 ^ivar1');
		updateInstanceMethodDefinition:  (RwMethodDefinition
						newForSelector: #'foo'
						protocol: 'accessing'
						source: 'foo ^2');
		comment: 'a new comment'.
	[ Rowan projectTools load loadProjectDefinition: projectDefinition ]
			on: RwExecuteClassInitializeMethodsAfterLoadNotification
			do: [:ex | ex resume: true ].

	theClass1 := Rowan globalNamed: 'Issue114Class_1'.
	theClass2 := Rowan globalNamed: 'Issue114Class_2'.
	self assert: theClass1 new foo = 1.
	self assert: theClass2 new foo = 2.
	self assert: theClass2 new ivar1 isNil.
	self assert: theClass2 comment = 'a new comment'.
