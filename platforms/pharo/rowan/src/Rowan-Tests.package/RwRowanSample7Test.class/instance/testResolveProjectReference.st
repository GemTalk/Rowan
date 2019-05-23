tests
testResolveProjectReference

	"clone a repository from github, attach to an existing git repository, clone to an alternate projectHome"

	| specUrlString rowanSpec projectHome projectName projectSpec projectReferenceDefinition_1 projectReferenceDefinition_2 informHappened |

	projectName := 'RowanSample7'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].
	(Rowan image projectRepositoryNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :repo | Rowan image _removeProjectRepository: repo ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	projectHome := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	specUrlString := self _rowanSample7SpecificationUrl.
	projectSpec := specUrlString asRwUrl asSpecification.

"create project reference definitions"
	projectReferenceDefinition_1 := RwProjectReferenceDefinition 
		newForSpecification: projectSpec 
		projectHome: projectHome.

	projectReferenceDefinition_1 repositoryRoot ensureDeleteAll.
	(Rowan image projectRepositoryNamed: projectReferenceDefinition_1 projectAlias ifAbsent: [  ])
		ifNotNil: [ :repo | Rowan image _removeProjectRepository: repo ].

"1. clone repository"
	self assert: (Rowan image projectRepositoryNamed: projectReferenceDefinition_1 projectAlias ifAbsent: [  ]) isNil.
	self 
		handleInformDuring: [ projectReferenceDefinition_1 resolve ] 
		interactionBlock: [:inform | self assert: false description: 'unexpected inform' ].

"validate"
	self assert: projectReferenceDefinition_1 repositoryRoot exists.
	self assert: (Rowan image projectRepositoryNamed: projectReferenceDefinition_1 projectAlias ifAbsent: [  ]) notNil.

"2. create second project reference definitions"
	projectReferenceDefinition_2 := RwProjectReferenceDefinition 
		newForSpecification: projectSpec 
		projectHome: projectHome.

"attach to repository - inform confirms that skip branch was taken"
	informHappened := false.
	self 
		handleInformDuring: [ projectReferenceDefinition_2 resolve ] 
		interactionBlock: [:inform | 
			"skipping the clone"
			Transcript cr; show: inform message.
			informHappened := true ].
	self assert: informHappened.

"validate"
	self assert: projectReferenceDefinition_1 projectHome = projectReferenceDefinition_2 projectHome.
	self assert: (Rowan image projectRepositoryNamed: projectReferenceDefinition_1 projectAlias ifAbsent: [  ]) == projectReferenceDefinition_2 repositoryDefinition. "expect repository defintion to be shared"

"3. now clone to a different spot by just changing the project home"
	projectReferenceDefinition_2 repositoryRoot ensureDeleteAll.
	(Rowan image projectRepositoryNamed: projectReferenceDefinition_2 projectAlias ifAbsent: [  ])
		ifNotNil: [ :repo | Rowan image _removeProjectRepository: repo ].

	projectHome := projectHome asFileReference / 'sample7_repos'.
	projectHome ensureDeleteAll.
	(Rowan image projectRepositoryNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :repo | Rowan image _removeProjectRepository: repo ].
	projectHome ensureCreateDirectory.

	projectReferenceDefinition_2 projectHome: projectHome.

"clone to new location"
	self assert: (Rowan image projectRepositoryNamed: projectReferenceDefinition_2 projectAlias ifAbsent: [  ]) isNil.
	self 
		handleInformDuring: [ projectReferenceDefinition_2 resolve ] 
		interactionBlock: [:inform | self assert: false dexcription: 'unexpected inform: ', inform message printString ].

"validate"
	self assert: projectReferenceDefinition_2 repositoryRoot exists.
	self deny: projectReferenceDefinition_1 repositoryRoot exists.	"confirm that the repo wasn't cloned to old location"
	self assert: (Rowan image projectRepositoryNamed: projectReferenceDefinition_2 projectAlias ifAbsent: [  ]) notNil.