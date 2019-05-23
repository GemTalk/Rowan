tests
testLoadProjectFromUrl_2

	| specUrlString projectTools rowanSpec gitRootPath projectName spec theClass |
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

	"load project into stone"
	projectTools load loadProjectFromSpecUrl: 'file:', gitRootPath pathString, '/', projectName, '/', spec specsPath, '/RowanSample4_load.ston'.

	theClass := Rowan globalNamed: 'RowanSample4'.

	self assert: theClass new foo = 'foo'