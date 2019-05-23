tests
testCreateNewProjectFromUrl

	"
		1. clone RowanSample7 using non-component API (v1.2.x style)
		2. read project from disk into a project definition
		3. create a new git project on disk using component API
		4. write the project definition to the new project.
	"

	| specUrlString rowanSpec gitRootPath projectName projectSpec_1 projectDefinition_2 projectDefinition_3 projectName_3 |

	projectName := 'RowanSample7'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].
	(Rowan image projectRepositoryNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :repo | Rowan image _removeProjectRepository: repo ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

"1. clone RowanSample7 using non-component API (v1.2.x style)"
	specUrlString :=  self _rowanSample7SpecificationUrl_12x.
	projectSpec_1 := specUrlString asRwUrl asSpecification.
	gitRootPath := rowanSpec repositoryRootPath asFileReference / 'test/testRepositories/repos/'.

	(gitRootPath / projectName) ensureDeleteAll.

	Rowan projectTools clone
		cloneSpecification: projectSpec_1
		gitRootPath: gitRootPath
		useSsh: true
		registerProject: false.	"does not register the project, so it is not visible in project list ... does however clone the project to local disk"

"validate"
	self assert: (Rowan image projectRepositoryNamed: projectSpec_1 specName ifAbsent: [  ]) isNil.

"2. read project from disk into a project definition, using component API (v2.0 style)"
	specUrlString := self _rowanSample7SpecificationUrl.
	projectDefinition_2 := (RwComponentProjectDefinition newForUrl: specUrlString)
		repositoryRoot: gitRootPath / projectName;
		yourself.

	projectDefinition_2 readProjectSet.

"3. create a new git project on disk using component API"

	projectName_3 := projectName, '_3'.

	(Rowan image projectRepositoryNamed: projectName_3 ifAbsent: [  ])
		ifNotNil: [ :repo | Rowan image _removeProjectRepository: repo ].

	projectDefinition_3 := RwComponentProjectDefinition
		projectName: projectName_3
			configurationNames: #( 'Main' ) 
			groupNames: #('core')  
			defaultComponentName: 'Main'
			useGit: true
			projectUrl: 'https://github.com/dalehenrich/RowanSample7' 
			projectHome: gitRootPath
			committish: 'candidateV1.0' 
			committishType: 'branch'
			comment: 'sample project created from scratch -- copied programatically from RowanSample7'.

	projectDefinition_3 repositoryRoot ensureDeleteAll.

	projectDefinition_3 create.

"validate"
	self assert: projectDefinition_3 repositoryRoot exists.
	self assert: (Rowan image projectRepositoryNamed: projectName_3 ifAbsent: [  ]) notNil.

"4. write the project definition to the new project location"

	projectDefinition_2 projectRef: projectDefinition_3 projectRef.
	projectDefinition_2 export.

"validate"
	self assert: (gitRootPath asFileReference / projectName_3 / 'rowan' / 'components' / 'Main', 'ston') exists