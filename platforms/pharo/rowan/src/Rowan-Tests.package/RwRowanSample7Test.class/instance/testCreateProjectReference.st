tests
testCreateProjectReference

	"Excercise RwProjectReferenceDefinition instance creation protocol ... 
		create the project reference on disk"

	| prd projectUrl projectName configurationNames groupNames comment projectHome
		rowanSpec|

	projectName := 'RowanSample7_test'.
	configurationNames := #( 'Main' ).
	groupNames := #( 'core' ).
	projectUrl := self _gitHubProjectUrl.
	comment := 'test project reference'.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].
	(Rowan image projectRepositoryNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :repo | Rowan image _removeProjectRepository: repo ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	projectHome := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

"create definition"
	prd := RwProjectReferenceDefinition
		projectName: projectName 
			configurationNames: configurationNames 
			groupNames: groupNames 
			useGit: true 
			projectUrl: projectUrl 
			comment: comment.
	prd projectHome: projectHome.

	prd repositoryRoot ensureDeleteAll.

"create project reference on disk"
	prd create.

"validate"
	self assert: prd repositoryRoot exists.
	self assert: prd configsRoot exists.
	self assert: prd specsRoot exists.
	self assert: prd packagesRoot exists.
	self assert: prd projectsRoot exists.

	self deny: prd configsRoot hasChildren.
	self deny: prd packagesRoot hasChildren.
	self deny: prd projectsRoot hasChildren.
	self assert: prd specsRoot hasChildren.

	self assert: (Rowan image loadedProjectNamed: projectName ifAbsent: [  ]) isNil.
	self assert: (Rowan image projectRepositoryNamed: projectName ifAbsent: [  ]) notNil.