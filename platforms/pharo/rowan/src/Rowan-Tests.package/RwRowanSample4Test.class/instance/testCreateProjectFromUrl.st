tests
testCreateProjectFromUrl

	| specUrlString projectTools rowanSpec gitRootPath projectName projectDefinition spec |
	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath asFileReference / 'test/testRepositories/repos/'.

	(gitRootPath / projectName) ensureDeleteAll.

	spec := specUrlString asRwUrl asSpecification.
	projectTools clone
		cloneSpecification: spec
		gitRootPath: gitRootPath
		useSsh: true
		registerProject: false.	"does not register the project, so it is not visible in project list ... does however clone the project to local disk"

	"attach a project definition to the Rowan project on disk ... not loaded and not registered"
	projectDefinition := projectTools create createProjectFromSpecUrl: 'file:', (gitRootPath / projectName / spec specsPath / 'RowanSample4_load.ston') pathString.

	self assert: (Rowan image loadedProjectNamed: projectName ifAbsent: []) notNil.

	projectTools load loadProjectNamed: projectName