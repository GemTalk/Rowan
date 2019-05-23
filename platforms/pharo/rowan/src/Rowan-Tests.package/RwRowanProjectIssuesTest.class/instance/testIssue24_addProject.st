tests-issue 24
testIssue24_addProject

	| projectName |
	projectName := 'Issue24_addProject'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject/'.

	self _writeProjectNamed: projectName
