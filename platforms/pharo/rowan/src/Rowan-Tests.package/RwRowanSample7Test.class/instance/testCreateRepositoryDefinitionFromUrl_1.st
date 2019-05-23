tests
testCreateRepositoryDefinitionFromUrl_1

	"exercise the RwAbstractRepositoryDefinition class creation protocol"

	| specUrlString rowanSpec gitRootPath projectName projectSpec repositoryDefinition_1 x y |

	projectName := 'RowanSample7'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	specUrlString := self _rowanSample7SpecificationUrl.
	projectSpec := specUrlString asRwUrl asSpecification.


"create repository definitions"
	repositoryDefinition_1 := RwAbstractRepositoryDefinition 
		newForSpecification: projectSpec 
			projectHome: gitRootPath.

	self assert: repositoryDefinition_1  name = projectSpec specName.
	self assert: repositoryDefinition_1  repositoryRoot = (gitRootPath asFileReference / projectName).
	self assert: (x := repositoryDefinition_1  committish) =  (y := 'v', self _projectVersionString).
	self assert: repositoryDefinition_1  projectUrl = self _gitHubProjectUrl