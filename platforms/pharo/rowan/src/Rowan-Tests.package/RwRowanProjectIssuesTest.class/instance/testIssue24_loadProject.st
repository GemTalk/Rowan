tests-issue 24
testIssue24_loadProject

	| projectName packageName className theClass project |
	projectName := 'Issue24_loadProject'.
	packageName := 'Issue24-Core'.
	className := 'Issue24Class'.

	self _createLoadedProjectNamed: projectName packageNames: {packageName} root: '/tmp/rowanIssuesProject/'  validate: false.
	theClass := self _addClassNamed: className toPackageNamed: packageName inProjectNamed: projectName  validate: false.
	self _addOrUpdateMethod:  'foo ^''foo''' forBehavior: theClass inPackage: packageName inProjectNamed: projectName  validate: false.
	self _addOrUpdateMethod:  'foo ^''foo''' forBehavior: theClass class inPackage: packageName inProjectNamed: projectName  validate: false.
	self _writeProjectNamed: projectName  validate: false.

	project := RwProject newNamed: projectName.

	self deny: project isDirty.

	Rowan projectTools load loadProjectNamed: projectName.

	self deny: project isDirty.

	self _addOrUpdateMethod:  'foo ^''bar''' forBehavior: theClass inPackage: packageName inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod:  'foo ^''bar''' forBehavior: theClass class inPackage: packageName inProjectNamed: projectName validate: false.

	self assert: project isDirty.
	self assert: theClass new foo = 'bar'.

	Rowan projectTools load loadProjectNamed: projectName.

	self deny: project isDirty.

	theClass := Rowan image objectNamed: className.
	self assert: theClass notNil.

	self _removeClassNamed: className fromProjectNamed: projectName.

	theClass := Rowan image objectNamed: className.
	self assert: theClass isNil.

	self assert: project isDirty.

	Rowan projectTools load loadProjectNamed: projectName.

	self deny: project isDirty.

	theClass := Rowan image objectNamed: className.
	self assert: theClass notNil.
	self assert: theClass new foo = 'foo'.
