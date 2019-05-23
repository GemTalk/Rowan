tests-issue 24
testIssue24_moveMethodBetweenProjects

	"move a method from one package to another"

	| projectName1 projectName2 packageName1 packageName2 className theClass |
	projectName1 := 'Issue24_moveMethodBetweenProjects1'.
	projectName2 := 'Issue24_moveMethodBetweenProjects2'.
	packageName1 := 'Issue24-Core1'.
	packageName2 := 'Issue24-Core2'.
	className := 'Issue24Class'.

	self _createLoadedProjectNamed: projectName1 root: '/tmp/rowanIssuesProject1/'.
	self _createLoadedProjectNamed: projectName2 root: '/tmp/rowanIssuesProject2/'.

	self _addPackageNamed: packageName1 toProjectNamed: projectName1 validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName2 validate: false.

	self _writeProjectNamed: projectName1.
	self _writeProjectNamed: projectName2.

	theClass := self _addClassNamed: className toPackageNamed: packageName1 inProjectNamed: projectName1.

	self _addOrUpdateMethod:  'foo ^''foo''' forBehavior: theClass inPackage: packageName1 inProjectNamed: projectName1 validate: false.

	self _writeProjectNamed: projectName1.

	self _addOrUpdateMethod:  'foo ^''foo''' forBehavior: theClass inPackage: packageName2 inProjectNamed: projectName2.

	self _writeProjectNamed: projectName1.
	self _writeProjectNamed: projectName2.
