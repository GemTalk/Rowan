Class {
	#name : 'RwRowanSample7Test',
	#superclass : 'RwToolTest',
	#instVars : [
		'repositoryRoot'
	],
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwRowanSample7Test >> _repositoryRoot [

	^ repositoryRoot
]

{ #category : 'private' }
RwRowanSample7Test >> _repositoryRoot: anObject [

	repositoryRoot := anObject
]

{ #category : 'private' }
RwRowanSample7Test >> _rowanSample7SpecificationUrl [

	| rowanSpec |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	^ 'file:' , rowanSpec repositoryRootPath , '/test/specs/RowanSample7.ston'
]

{ #category : 'running' }
RwRowanSample7Test >> tearDown [

	self _repositoryRoot ifNotNil: [:root | root ensureDelete ].
	super tearDown
]

{ #category : 'tests' }
RwRowanSample7Test >> testCreateProjectFromUrl [

	| specUrlString projectTools rowanSpec gitRootPath projectName projectDefinition spec repositoryDefinition |
	projectName := 'RowanSample7'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample7SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	spec := specUrlString asRwUrl asSpecification.
	repositoryDefinition := RwDiskRepositoryDefinition newForSpecification: spec projectHome: gitRootPath.

"delete repository from disk if it exists"
	repositoryDefinition repositoryExists
		ifTrue: [ repositoryDefinition repositoryRoot deleteAll ].

self error: 'test not complete ...'.

	projectTools clone
		cloneSpecification: spec
		gitRootPath: gitRootPath
		useSsh: true
		registerProject: false.	"does not register the project, so it is not visible in project list ... does however clone the project to local disk"

	"attach a project definition to the Rowan project on disk ... not loaded and not registered"
	projectDefinition := projectTools create createProjectFromSpecUrl: 'file:', gitRootPath, '/', projectName, '/', spec specsPath, '/RowanSample4_load.ston'.

	self assert: (Rowan image loadedProjectNamed: projectName ifAbsent: []) notNil.

	projectTools load loadProjectNamed: projectName
]

{ #category : 'tests' }
RwRowanSample7Test >> testCreateRepositoryDefinitionFromUrl_1 [

	| specUrlString rowanSpec gitRootPath projectName componentSpec repositoryDefinition_1
		repositoryDefinition_2 repositoryDefinition_3 repositoryDefinition_4 |
	projectName := 'RowanSample7'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	specUrlString := self _rowanSample7SpecificationUrl.
	componentSpec := specUrlString asRwUrl asSpecification.


"create repository definitions"
	repositoryDefinition_1 := RwAbstractRepositoryDefinition 
		newForSpecification: componentSpec 
			projectHome: gitRootPath.

	repositoryDefinition_2 := RwAbstractRepositoryDefinition 
		newForSpecification: componentSpec 
			projectNamed: projectName
			projectHome: gitRootPath.

	repositoryDefinition_3 := RwAbstractRepositoryDefinition 
		newNamed: projectName 
			forSpecification: componentSpec 
			projectHome: gitRootPath.

	repositoryDefinition_4 := RwAbstractRepositoryDefinition 
		newNamed: projectName 
			forSpecification: componentSpec
			projectNamed: projectName
			projectHome: gitRootPath.

	{repositoryDefinition_1 . repositoryDefinition_2 . repositoryDefinition_3 . repositoryDefinition_4}
		do: [:repoDef |
			self assert: repoDef  configsPath = repositoryDefinition_1 configsPath.
			self assert: repoDef  name = repositoryDefinition_1 name.
			self assert: repoDef  repositoryRoot = repositoryDefinition_1 repositoryRoot.
			self assert: repoDef  specsPath = repositoryDefinition_1 specsPath.
			self assert: repoDef  srcPath = repositoryDefinition_1 srcPath ]
]

{ #category : 'tests' }
RwRowanSample7Test >> testCreateRepositoryDefinitionFromUrl_2 [

	| specUrlString rowanSpec gitRootPath projectName componentSpec repositoryDefinition x y  |
	projectName := 'RowanSample7'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	gitRootPath := FileLocator rowanProjectsHome resolve / 'Rowan/test/testRepositories/repos/'.

	specUrlString := self _rowanSample7SpecificationUrl.
	componentSpec := specUrlString asRwUrl asSpecification.

	self _repositoryRoot: FileSystem memory * 'repoDefinitionTestRoot'.

"create repository definition based on disk"
	repositoryDefinition := RwAbstractRepositoryDefinition 
		newForSpecification: componentSpec 
			projectHome: gitRootPath.

"validate"
	self assert: (x := repositoryDefinition srcPath pathString) = (y := (gitRootPath / projectName / componentSpec repoPath) pathString),

self error: 'test not complete'.

repositoryDefinition repositoryRootPath: 'a memory reference'
]
