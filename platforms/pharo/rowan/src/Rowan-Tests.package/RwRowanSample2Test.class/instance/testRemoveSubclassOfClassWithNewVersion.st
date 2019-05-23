tests
testRemoveSubclassOfClassWithNewVersion

	"#RowanSample2 project
		## branch migration_1
			class structure:
				RowanSample2
				    instVarNames: #( ivar0)
				 RowanSample2A
				    instVarNames: #( a ivar1)
				  RowanSample2C
				    instVarNames: #( c ivar2)
				  RowanSample2D
				    instVarNames: #( d ivar2)
				 RowanSample2B
				    instVarNames: #( b ivar1)
				  RowanSample2E
				    instVarNames: #( e ivar2)
				  RowanSample2F
				    instVarNames: #( f ivar2)
		## branch migration_0
			class structure:
				RowanSample2
"

	"load migration_1, then load migration_0 (new version of RowanSample2 and all subclasses deleted"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSample2Class subclasses rowanSampleSpec |
	projectName := 'RowanSample2'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample2SpecificationUrl.
	projectTools := Rowan projectTools.
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
	projectTools load loadProjectNamed: projectName withConfigurations: #( 'Default' ) groupNames: #().

	rowanSample2Class := Rowan globalNamed: 'RowanSample2'.
	subclasses := ClassOrganizer new allSubclassesOf: rowanSample2Class.
	self assert: subclasses size = 6.

	gitTool gitcheckoutIn: gitRootPath with: 'migration_0'.
	projectTools load loadProjectNamed: projectName withConfigurations: #( 'Default' ) groupNames: #().

	rowanSample2Class := Rowan globalNamed: 'RowanSample2'.
	subclasses := ClassOrganizer new allSubclassesOf: rowanSample2Class.
	self assert: subclasses size = 0