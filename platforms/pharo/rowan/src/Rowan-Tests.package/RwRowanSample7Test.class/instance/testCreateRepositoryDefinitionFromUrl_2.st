tests
testCreateRepositoryDefinitionFromUrl_2

	"verify that the repository root can be dynamically swapped out (if desired) ... repository definitioin can be ointed wholesale to another $ROWAN_PROJECTS_HOME"

	| specUrlString rowanSpec gitRootPath projectName projectSpec repositoryDefinition x y  memoryRoot repositoryDefinition_1 |

	projectName := 'RowanSample7'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	gitRootPath := FileLocator rowanProjectsHome resolve / 'Rowan/test/testRepositories/repos/'.

	specUrlString := self _rowanSample7SpecificationUrl.
	projectSpec := specUrlString asRwUrl asSpecification.

"create repository definition based on disk"
	repositoryDefinition := RwDiskRepositoryDefinition 
		newForSpecification: projectSpec 
			projectHome: gitRootPath.

	self assert: repositoryDefinition  repositoryRoot fileSystem isDiskFileSystem.
	self assert: repositoryDefinition class == RwDiskRepositoryDefinition.

"validate"
	self assert: (x := repositoryDefinition repositoryRoot pathString) = (y := (gitRootPath / projectName) pathString).

"create repository definition based on memory"
	memoryRoot := FileSystem memory * 'repoDefinitionTestRoot'.
	self _repositoryRoot: memoryRoot.

	gitRootPath := memoryRoot / 'repos'.
	repositoryDefinition repositoryRoot: gitRootPath / projectName.

"validate"
	self assert: repositoryDefinition  repositoryRoot fileSystem isMemoryFileSystem.
	self assert: (x := repositoryDefinition repositoryRoot pathString) = (y := (gitRootPath / projectName) pathString).

"create from scratch and it should be using RwMemoryRepositoryDefinition"
	repositoryDefinition_1 := RwAbstractRepositoryDefinition 
		newForSpecification: projectSpec 
			projectHome: gitRootPath.

"validate"
	self assert: repositoryDefinition_1  repositoryRoot fileSystem isMemoryFileSystem.
	self assert: repositoryDefinition_1 class == RwMemoryRepositoryDefinition.

	self assert: repositoryDefinition  name = repositoryDefinition_1 name.
	self assert: repositoryDefinition  repositoryRoot = repositoryDefinition_1 repositoryRoot.