tests
testDeferredMigration

	"load migration_1, set all of the instance variables (a-f, ivar0-ivar2), then load migration_2. after deferred migration ..."

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec instanceMigrator 
		classesToMigrate expectedClassesToMigrate |
	projectName := 'RowanSample2'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

  	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample2SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath asFileReference / 'test/testRepositories/repos/'.

	(gitRootPath / projectName) ensureDeleteAll.

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: rowanSpec repositoryRootPath asFileReference / 'test/testRepositories/repos/'
		useSsh: true.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	(rowanSampleSpec platformSpec at: 'gemstone')
		projectOwnerId: Rowan image currentUserId;
		defaultSymbolDictName: self _symbolDictionaryName;
		yourself.

	gitRootPath := rowanSampleSpec repositoryRootPath asFileReference.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: gitRootPath with: 'migration_1'.
	projectTools load
		loadProjectNamed: projectName
		withConfigurations: #( 'Default' )
		groupNames: #()
		instanceMigrator: RwGsInstanceMigrator noMigration.

	gitTool gitcheckoutIn: gitRootPath with: 'migration_2'.
	instanceMigrator := RwGsDeferredInstanceMigrator noMigration.
	projectTools load
		loadProjectNamed: projectName
		withConfigurations: #( 'Default' )
		groupNames: #()
		instanceMigrator: instanceMigrator.

	classesToMigrate := (instanceMigrator classesToMigrate collect: [:each | each name ]) sort.
	expectedClassesToMigrate := (self _migrationClassMap collect: [:each | each at: 1 ]) sort.
	self assert: classesToMigrate = expectedClassesToMigrate