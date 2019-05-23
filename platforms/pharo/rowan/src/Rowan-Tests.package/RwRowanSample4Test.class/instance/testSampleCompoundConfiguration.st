tests
testSampleCompoundConfiguration

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec project x |
	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath asFileReference / 'test/testRepositories/repos/'.

	(gitRootPath / projectName) ensureDeleteAll.

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: true.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	(rowanSampleSpec platformSpec at: 'gemstone')
		projectOwnerId: Rowan image currentUserId;
		defaultSymbolDictName: self _symbolDictionaryName;
		yourself.

	gitRootPath := rowanSampleSpec repositoryRootPath asFileReference.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: gitRootPath with: 'master'.
	projectTools load
		loadProjectNamed: projectName
		withConfigurations: #( 'Test' )
		groupNames: #()
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	self
		assert:
			(x := project packageNames asArray sort)
				= #('RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests') sort