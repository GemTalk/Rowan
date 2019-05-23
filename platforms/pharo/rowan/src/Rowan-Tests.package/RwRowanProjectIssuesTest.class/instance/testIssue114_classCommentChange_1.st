tests-issue 114
testIssue114_classCommentChange_1

	"https://github.com/dalehenrich/Rowan/issues/114"

	"new class version of superclass, where both superclass and subclass have a class comment, change superclass 
		comment"

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
	theClass1 rwComment: 'theClass1 comment'.

	theClass2 := self 
		_addClassNamed: 'Issue114Class_2' 
		superclassName: 'Issue114Class_1' 
		classVars: #(  )
		toPackageNamed: packageName2
		inProjectNamed: projectName
		validate: false.
	theClass2 rwComment: 'theClass2 comment'.

	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName1) classDefinitions at:  'Issue114Class_1'.
	classDefinition 
		instVarNames: #(ivar1);	"new class version"
		comment: 'a new comment'.
	classDefinition := (projectDefinition packageNamed: packageName2) classDefinitions at:  'Issue114Class_2'.

	[ Rowan projectTools load loadProjectDefinition: projectDefinition ]
			on: RwExecuteClassInitializeMethodsAfterLoadNotification
			do: [:ex | ex resume: true ].

	theClass1 := Rowan globalNamed: 'Issue114Class_1'.
	theClass2 := Rowan globalNamed: 'Issue114Class_2'.
	self assert: theClass1 comment = 'a new comment'.
	self assert: theClass2 comment = 'theClass2 comment'.
