tests-issue 114
testIssue114_bothModificationsMustShareCommonAfter_2

	"https://github.com/dalehenrich/Rowan/issues/114"

	"new class version of superclass and changed comment in subclass"

	| projectName packageName1 packageName2 theClass1 theClass2 projectDefinition classDefinition comment |
	projectName := 'Issue114_project'.
	packageName1 := 'Issue114-Core1'.
	packageName2 := 'Issue114-Core2'.
	comment := 'a class comment'.

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

	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName1) classDefinitions at:  'Issue114Class_1'.
	classDefinition instVarNames: #(ivar1).	"new class version"
	classDefinition := (projectDefinition packageNamed: packageName2) classDefinitions at:  'Issue114Class_2'.
	classDefinition comment: comment.
	[ Rowan projectTools load loadProjectDefinition: projectDefinition ]
			on: RwExecuteClassInitializeMethodsAfterLoadNotification
			do: [:ex | ex resume: true ].

	theClass2 := Rowan globalNamed: 'Issue114Class_2'.
	self assert: theClass2 comment = comment.
