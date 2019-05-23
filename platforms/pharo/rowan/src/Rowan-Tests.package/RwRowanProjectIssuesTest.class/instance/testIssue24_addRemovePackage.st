tests-issue 24
testIssue24_addRemovePackage

	| projectName packageName |
	projectName := 'Issue24_addRemovePackage'.
	packageName := 'Issue24-Core'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject/'.

	self _writeProjectNamed: projectName.

	self _addPackageNamed: packageName toProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _removePackageNamed: packageName from: projectName.

	self _writeProjectNamed: projectName.
