tests
test_projectUrl_issue_463

	| specUrlString projectTools rowanSpec gitRootPath projectName rowanSampleSpec project |
	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: true.

	project := RwProject newNamed: projectName.
	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.

	self assert: project projectUrl = rowanSampleSpec projectUrl.
	self assert: project projectUrl = 'https://github.com/dalehenrich/RowanSample4'.

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	self assert: project projectUrl = 'https://github.com/dalehenrich/RowanSample4'.