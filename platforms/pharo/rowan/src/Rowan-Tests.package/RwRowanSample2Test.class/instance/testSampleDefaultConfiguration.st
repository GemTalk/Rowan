tests
testSampleDefaultConfiguration

	"SampleDefault configuration is an instance of RwProjectConfiguration"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec project x |
	projectName := 'RowanSample2'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample2SpecificationUrl.
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
	gitTool gitcheckoutIn: gitRootPath with: 'migration_0'.
	projectTools load loadProjectNamed: projectName withConfigurations: #( 'Default' ) groupNames: #() instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	self assert: (x := project packageNames) = #('RowanSample2-Core')