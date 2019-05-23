private-issue 150
_cloneGitRepositoryFor: projectName projectUrlString: projectUrlString

	| rowanSpec projectTools gitRootPath |
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath asFileReference / 'test/testRepositories/repos/issues/'.

	(gitRootPath / projectName) ensureDeleteAll.

	projectTools clone
		cloneSpecUrl: projectUrlString
		gitRootPath: rowanSpec repositoryRootPath , '/test/testRepositories/repos/'
		useSsh: true.