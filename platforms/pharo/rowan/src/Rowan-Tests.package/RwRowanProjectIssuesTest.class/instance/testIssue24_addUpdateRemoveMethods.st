tests-issue 24
testIssue24_addUpdateRemoveMethods

	| projectName packageName className theClass |
	projectName := 'Issue24_addUpdateRemoveMethods'.
	packageName := 'Issue24-Core'.
	className := 'Issue24Class'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject/'.

	self _writeProjectNamed: projectName.

	self _addPackageNamed: packageName toProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	theClass := self _addClassNamed: className toPackageNamed: packageName inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _addOrUpdateMethod:  'foo ^''foo''' forBehavior: theClass inPackage: packageName inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _addOrUpdateMethod:  'foo ^''foo''' forBehavior: theClass class inPackage: packageName inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _addOrUpdateMethod:  'foo ^''bar''' forBehavior: theClass inPackage: packageName inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _addOrUpdateMethod:  'foo ^''bar''' forBehavior: theClass class inPackage: packageName inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _removeSelector: #foo fromBehavior: theClass inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _removeSelector: #foo fromBehavior: theClass class inProjectNamed: projectName.

	self _writeProjectNamed: projectName.
