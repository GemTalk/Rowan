tests
testResolveRepositoryDefinition

	"clone a repository from github using repositoryDefinition"

	| specUrlString rowanSpec gitRootPath projectName projectSpec repositoryDefinition |

	projectName := 'RowanSample7'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	gitRootPath := (rowanSpec repositoryRootPath , '/test/testRepositories/repos/') asFileReference.

	specUrlString := self _rowanSample7SpecificationUrl.
	projectSpec := specUrlString asRwUrl asSpecification.

"create repository definitions using standard git repo name"
	repositoryDefinition := RwAbstractRepositoryDefinition 
		newForSpecification: projectSpec 
			projectHome: gitRootPath.

	repositoryDefinition repositoryRoot ensureDeleteAll.

"clone repository "
	self 
		handleInformDuring: [ repositoryDefinition resolve ] 
		interactionBlock: [:inform | self assert: false description: 'unexpected inform' ].

"validate"
	self assert: gitRootPath exists.
	self assert: repositoryDefinition repositoryExists.

	repositoryDefinition repositoryRoot ensureDeleteAll.

"create repository definitions using alternat git repo name"
	projectName := projectName, '_alt'.
	repositoryDefinition := RwGitRepositoryDefinition 
		newNamed: projectName 
			forSpecification: projectSpec 
			repositoryRoot: gitRootPath / projectName.

	repositoryDefinition repositoryRoot ensureDeleteAll.

"clone repository"
	self 
		handleInformDuring: [ repositoryDefinition resolve ] 
		interactionBlock: [:inform | self assert: false description: 'unexpected inform' ].

"validate"
	self assert: gitRootPath exists.
	self assert: repositoryDefinition repositoryExists.