Extension { #name : 'RwRowanProjectIssuesTest' }

{ #category : '*rowan-tests-components' }
RwRowanProjectIssuesTest >> _cloneGitRepositoryFor: projectName projectUrlString: projectUrlString [

	| rowanProject projectTools gitRootPath |
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	projectTools := Rowan projectTools.

	gitRootPath := self _testRowanProjectsSandbox.

	(gitRootPath / projectName) ensureDeleteAll.

	(RwComponentProjectDefinition newForUrl: projectUrlString)
		projectHome: gitRootPath;
		clone;
		register.
]
