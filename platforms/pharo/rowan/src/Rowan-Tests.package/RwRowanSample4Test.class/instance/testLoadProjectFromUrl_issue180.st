tests
testLoadProjectFromUrl_issue180

	"https://github.com/dalehenrich/Rowan/issues/180"

	| specUrlString projectTools rowanSpec gitRootPath projectName spec |
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

	"load project into stone, again ... should be legal"
	projectTools load loadProjectFromSpecUrl: 'file:', gitRootPath pathString, '/', projectName, '/', spec specsPath, '/RowanSample4_load.ston'.