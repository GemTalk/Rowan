Extension { #name : 'RwRowanSample4Test' }

{ #category : '*rowan-tests-32x' }
RwRowanSample4Test >> _createProjectDefinitionFromSpecUrl: specUrlString [

	^ Rowan projectTools create createProjectFromSpecUrl: specUrlString
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
