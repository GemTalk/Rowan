tests-issue 24
testIssue24_moveMethodBetweenPackages

	"move a method from one package to another"

	| projectName packageName1 packageName2 className theClass |

	projectName := 'Issue24_moveMethodBetweenPackages'.
	packageName1 := 'Issue24-Core1'.
	packageName2 := 'Issue24-Core2'.
	className := 'Issue24Class'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject/'.

	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	self _writeProjectNamed: projectName.

	theClass := self _addClassNamed: className toPackageNamed: packageName1 inProjectNamed: projectName.

	self _addOrUpdateMethod:  'foo ^''foo''' forBehavior: theClass inPackage: packageName1 inProjectNamed: projectName validate: false.

	self _writeProjectNamed: projectName.

	self _addOrUpdateMethod:  'foo ^''foo''' forBehavior: theClass inPackage: packageName2 inProjectNamed: projectName.

	self _writeProjectNamed: projectName.
