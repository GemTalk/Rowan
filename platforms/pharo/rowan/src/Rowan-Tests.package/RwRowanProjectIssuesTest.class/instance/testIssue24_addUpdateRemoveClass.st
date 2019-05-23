tests-issue 24
testIssue24_addUpdateRemoveClass

	| projectName packageName className  |
	projectName := 'Issue24_addUpdateRemoveClass'.
	packageName := 'Issue24-Core'.
	className := 'Issue24Class'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject/'.

	self _writeProjectNamed: projectName.

	self _addPackageNamed: packageName toProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _addClassNamed: className toPackageNamed: packageName inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _updateClassNamed: className toPackageNamed: packageName inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _removeClassNamed: className fromProjectNamed: projectName.

	self _writeProjectNamed: projectName.
