tests-issue 41
testIssue41_interactiveMoveInitializeExtensionMethodToPackage

	"https://github.com/dalehenrich/Rowan/issues/41"

	"Are extension method initialization methods run correctly for add and update"

	| projectName packageName1 packageName2 packageName3 className theClass loadedPackage loadedClassExtensions |
	projectName := 'Issue41_project'.
	packageName1 := 'Issue41-Core'.
	packageName2 := 'Issue41-Extensions1'.
	packageName3 := 'Issue41-Extensions2'.
	className := 'Issue41Class'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject/' validate: false.
	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName3 toProjectNamed: projectName validate: false.
	theClass := self _updateClassNamed: className toPackageNamed: packageName1 inProjectNamed: projectName validate: false.

	self _addOrUpdateMethod: 'cvar1 ^CVar1' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod: 'cvar2 ^CVar2' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod: 'cvar1: anInt CVar1 := anInt' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod: 'cvar2: anInt CVar2 := anInt' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.
	
	"create initialize package as an extension method"
	self _addOrUpdateMethod: 'initialize CVar1 := 1' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.

	self assert: theClass cvar1 = 1.
	self assert: theClass cvar2 isNil.

	theClass 
		cvar1: 3;
		cvar2: 4.
	self assert: theClass cvar1 = 3.
	self assert: theClass cvar2 = 4.

	"initialize method changed in same package"
	self _addOrUpdateMethod: 'initialize "" CVar1 := 1. CVar2 := 2' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.

	self assert: theClass cvar1 = 1.
	self assert: theClass cvar2 = 2.

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) classMethodDefinitions includesKey: #initialize).

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: (loadedClassExtensions at: className ifAbsent: []) isNil.

	theClass 
		cvar1: 5;
		cvar2: 6.
	self assert: theClass cvar1 = 5.
	self assert: theClass cvar2 = 6.

	"initialize method not changed and moved to new package"
	self _addOrUpdateMethod: 'initialize "" CVar1 := 1. CVar2 := 2' forBehavior: theClass class inPackage: packageName3 inProjectNamed: projectName validate: false.

	self assert: theClass cvar1 = 5.
	self assert: theClass cvar2 = 6.

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) classMethodDefinitions includesKey: #initialize).

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self deny: ((loadedClassExtensions at: className) classMethodDefinitions includesKey: #initialize).

	theClass 
		cvar1: 5;
		cvar2: 6.
	self assert: theClass cvar1 = 5.
	self assert: theClass cvar2 = 6.

	"initialize method not changed and left in same package"
	self _addOrUpdateMethod: 'initialize "" CVar1 := 1. CVar2 := 2' forBehavior: theClass class inPackage: packageName3 inProjectNamed: projectName validate: false.

	self assert: theClass cvar1 = 5.
	self assert: theClass cvar2 = 6.

	"initialize method not changed and moved to new package"
	self _addOrUpdateMethod: 'initialize "" CVar1 := 1. CVar2 := 2' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.

	self assert: theClass cvar1 = 5.
	self assert: theClass cvar2 = 6.

	theClass initialize.

	self assert: theClass cvar1 = 1.
	self assert: theClass cvar2 = 2.

	theClass 
		cvar1: 5;
		cvar2: 6.
	self assert: theClass cvar1 = 5.
	self assert: theClass cvar2 = 6.

	"initialize method changed and moved to new package"
	self _addOrUpdateMethod: 'initialize CVar1 := 1. CVar2 := 2' forBehavior: theClass class inPackage: packageName3 inProjectNamed: projectName validate: false.

	self assert: theClass cvar1 = 1.
	self assert: theClass cvar2 = 2.
