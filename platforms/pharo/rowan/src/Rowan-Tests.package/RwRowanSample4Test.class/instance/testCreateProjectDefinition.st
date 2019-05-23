tests
testCreateProjectDefinition

	| specUrlString projectTools rowanSpec gitRootPath projectName projectDefinition x |
	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath asFileReference / 'test/testRepositories/repos/'.

	(gitRootPath / projectName) ensureDeleteAll.

	projectTools clone
		cloneSpecification: specUrlString asRwUrl asSpecification
		gitRootPath: gitRootPath
		useSsh: true
		registerProject: false.	"does not register the project, so it is not visible in project list ... does however clone the project to local disk"

	"attach a project definition to the Rowan project on disk ... not loaded and not registered"
	projectDefinition := projectTools create createProjectDefinitionFromSpecUrl: specUrlString projectRootPath: gitRootPath / projectName.

	self assert: projectDefinition packageNames isEmpty.
	
	projectTools read readProjectDefinition: projectDefinition.

	self assert: (projectDefinition projectDefinitionSourceProperty = RwLoadedProject _projectDiskDefinitionSourceValue).

	self assert: (x := projectDefinition packageNames asArray sort) = #('RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-GemStone' 'RowanSample4-GemStone-Tests' 'RowanSample4-Tests') sort.

	self assert: (Rowan image loadedProjectNamed: projectName ifAbsent: []) isNil.