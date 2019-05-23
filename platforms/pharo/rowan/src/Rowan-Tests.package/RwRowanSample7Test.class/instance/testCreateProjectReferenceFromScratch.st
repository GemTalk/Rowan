tests
testCreateProjectReferenceFromScratch

	| rowanSpec gitRootPath projectName projectReferenceDefinition projectSpec |

	projectName := 'RowanSample7'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].
	(Rowan image projectRepositoryNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :repo | Rowan image _removeProjectRepository: repo ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	projectSpec := RwComponentSpecification new
		specName: projectName;
		repoSpec: RwDiskRepositorySpecification new; "want to create a disk-based repository"
		yourself.

"create project reference definitions"
	projectReferenceDefinition := RwProjectReferenceDefinition 
		projectName: projectName 
			configurationNames: #( 'Default'  )
			groupNames: #( 'core' )
			useGit: true
			projectUrl: self _gitHubProjectUrl
			comment: ''.

	projectReferenceDefinition projectHome: gitRootPath.

"validate"
	self assert: projectReferenceDefinition projectName = projectName.
	self assert: projectReferenceDefinition projectAlias = projectReferenceDefinition projectName.
	self assert: projectReferenceDefinition configurationNames = #( 'Default').
	self assert: projectReferenceDefinition groupNames = #( 'core').
	self assert: projectReferenceDefinition committish = 'master'.
	self assert: projectReferenceDefinition committishType = 'branch'.
	self assert: projectReferenceDefinition projectUrl = self _gitHubProjectUrl.

	self assert: projectReferenceDefinition projectHome pathString = gitRootPath asFileReference pathString.