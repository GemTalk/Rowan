tests-issue 24
testIssue24_moveClassBetweenPackages

	"move a class from one package to another"

	| projectName packageName1 packageName2 className testClass |
	projectName := 'Issue24_moveClassBetweenPackages'.
	packageName1 := 'Issue24-Core1'.
	packageName2 := 'Issue24-Core2'.
	className := 'Issue24Class'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject/'.

	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	self _writeProjectNamed: projectName.

	self _addClassNamed: className toPackageNamed: packageName1 inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	testClass := Rowan globalNamed: className.
	self assert: testClass rowanPackageName = packageName1.

	self _updateClassNamed: className toPackageNamed: packageName2 inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	testClass := Rowan globalNamed: className.
	self assert: testClass rowanPackageName = packageName2
