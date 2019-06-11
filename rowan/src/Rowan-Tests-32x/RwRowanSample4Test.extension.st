Extension { #name : 'RwRowanSample4Test' }

{ #category : '*rowan-tests-32x' }
RwRowanSample4Test >> _cloneProjectFromSpecUrl: specUrlString projectsHome: projectsHome [

	Rowan projectTools clone
		cloneSpecification: specUrlString asRwUrl asSpecification
		gitRootPath: projectsHome
		useSsh: true
		registerProject: true.
]

{ #category : '*rowan-tests-32x' }
RwRowanSample4Test >> _createProjectDefinitionFromSpecUrl: specUrlString projectName: projectName [

	| gitRootPath spec rowanProject|
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.

	gitRootPath := rowanProject repositoryRootPath asFileReference / 'test/testRepositories/repos/'.

	(gitRootPath / projectName) ensureDeleteAll.

	spec := specUrlString asRwUrl asSpecification.
	Rowan projectTools clone
		cloneSpecification: spec
		gitRootPath: gitRootPath
		useSsh: true
		registerProject: false.	"does not register the project, so it is not visible in project list ... does however clone the project to local disk"

	"attach a project definition to the Rowan project on disk ... not loaded and not registered"
	^ Rowan projectTools create createProjectFromSpecUrl: 'file:', (gitRootPath / projectName / spec specsPath / 'RowanSample4_load.ston') pathString.
]

{ #category : '*rowan-tests-32x' }
RwRowanSample4Test >> _loadProjectFromSpecUrl: specUrlString repoRootPath: repoRootPath [

	^ Rowan projectTools load
		loadProjectFromSpecUrl: specUrlString
		projectRootPath: repoRootPath
]

{ #category : '*rowan-tests-32x' }
RwRowanSample4Test >> _projectDefinitionFromSpecUrl: specUrlString projectRootPath: projectRootPath [

	| projectDefinition |

	Rowan projectTools clone
		cloneSpecification: specUrlString asRwUrl asSpecification
		gitRootPath: projectRootPath parent
		useSsh: true
		registerProject: false.	"does not register the project, so it is not visible in project list ... does however clone the project to local disk"

	projectDefinition := Rowan projectTools create createProjectDefinitionFromSpecUrl: specUrlString projectRootPath: projectRootPath.

	projectDefinition read.

	^ projectDefinition
]

{ #category : '*rowan-tests-32x' }
RwRowanSample4Test >> _rowanSample4_208_LoadSpecificationUrl [

	| rowanProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ 'file:' , rowanProject repositoryRootPath , '/test/specs/RowanSample4_208_load.ston'
]

{ #category : '*rowan-tests-32x' }
RwRowanSample4Test >> _rowanSample4LoadSpecificationUrl [

	| rowanProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ 'file:' , rowanProject repositoryRootPath , '/test/specs/RowanSample4_load.ston'
]

{ #category : '*rowan-tests-32x' }
RwRowanSample4Test >> _rowanSample4LoadSpecificationUrl_295 [

	| rowanProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ 'file:' , rowanProject repositoryRootPath , '/test/specs/RowanSample4_295.ston'
]

{ #category : '*rowan-tests-32x' }
RwRowanSample4Test >> _rowanSample4SpecificationUrl [

	| rowanProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ 'file:' , (rowanProject repositoryRootPath asFileReference / 'test/specs/RowanSample4.ston') pathString
]
