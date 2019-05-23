tests-issue 41
testIssue41_interactiveAddUpdateInitializeExtensionMethods

	"https://github.com/dalehenrich/Rowan/issues/41"

	"Are extension method initialization methods run correctly for add and update - interactive edits should not trigger updates "

	| projectName packageName1 packageName2 className theClass  |
	projectName := 'Issue41_project'.
	packageName1 := 'Issue41-Core'.
	packageName2 := 'Issue41-Extensions'.
	className := 'Issue41Class'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject/' validate: false.
	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.
	theClass := self _updateClassNamed: className toPackageNamed: packageName1 inProjectNamed: projectName validate: false.

	self _addOrUpdateMethod: 'cvar1 ^CVar1' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod: 'cvar2 ^CVar2' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.
	
	self _addOrUpdateMethod: 'initialize CVar1 := 1' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.

	self assert: theClass cvar1 = 1.
	self assert: theClass cvar2 isNil.

	self _addOrUpdateMethod: 'initialize CVar1 := 1. CVar2 := 2' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.

	self assert: theClass cvar1 = 1.
	self assert: theClass cvar2 = 2.
