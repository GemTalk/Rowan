Class {
	#name : 'RwRowanSample7Test',
	#superclass : 'RwToolTest',
	#instVars : [
		'repositoryRoot'
	],
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwRowanSample7Test >> _gitHubProjectUrl [

	^ 'https://github.com/dalehenrich/RowanSample7'
]

{ #category : 'private' }
RwRowanSample7Test >> _projectVersionString [

	^ '0.0.2'
]

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
		ifTrue: [ repositoryDefinition repositoryRoot ensureDeleteAll ].

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
RwRowanSample7Test >> testCreateProjectReferenceFromUrl [

	| specUrlString rowanSpec gitRootPath projectName projectSpec projectReferenceDefinition x y |

	projectName := 'RowanSample7'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	specUrlString := self _rowanSample7SpecificationUrl.
	projectSpec := specUrlString asRwUrl asSpecification.

"create project reference definitions"
	projectReferenceDefinition := RwAbstractProjectReferenceDefinition 
		newForSpecification: projectSpec 
		projectHome: gitRootPath.

"validate"
	self assert: projectReferenceDefinition projectName = projectName.
	self assert: projectReferenceDefinition projectAlias = projectReferenceDefinition projectName.
	self assert: projectReferenceDefinition configurationNames = #( 'Default').
	self assert: projectReferenceDefinition groupNames = #( 'core').
	self assert: projectReferenceDefinition projectUrl = 'https://github.com/dalehenrich/RowanSample7'.

	self assert: (x := projectReferenceDefinition projectHome pathString) = (y := gitRootPath asFileReference pathString).

"create project reference definitions"
	projectReferenceDefinition := RwAbstractProjectReferenceDefinition newForSpecification: projectSpec.

"validate"
	self assert: (x := projectReferenceDefinition projectHome pathString) = (y := FileLocator rowanProjectsHome pathString).
]

{ #category : 'tests' }
RwRowanSample7Test >> testCreateRepositoryDefinition [

	"exercise the RwAbstractRepositoryDefinition class creation protocol"

	| rowanSpec gitRootPath repositoryName repositoryDefinition_1  repositoryDefinition_2 repoRoot
		projectVersionString gitHubProjectUrl |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	repositoryName := 'RowanSample7'.
	repoRoot := gitRootPath asFileReference / repositoryName.
	projectVersionString := self _projectVersionString.
	gitHubProjectUrl := self _gitHubProjectUrl.

"create repository definitions using #newNamed:repositoryRoot:"
	repositoryDefinition_1 := RwAbstractRepositoryDefinition
		newNamed: repositoryName 
		repositoryRoot: repoRoot.

	repositoryDefinition_2 := RwAbstractRepositoryDefinition
		newNamed: repositoryName 
		repositoryRoot: repoRoot pathString.

"validate"
	self assert: repositoryDefinition_1  name = repositoryDefinition_2 name.
	self assert: repositoryDefinition_1  repositoryRoot = repositoryDefinition_2 repositoryRoot.
	self assert:  repositoryDefinition_1 repositoryRoot = repoRoot asFileReference.

"create repository definitions using #newNamed:repositoryRoot:version:"
	repositoryDefinition_1 := RwAbstractRepositoryDefinition
		newNamed: repositoryName 
		repositoryRoot: repoRoot
		version: projectVersionString.

	repositoryDefinition_2 := RwAbstractRepositoryDefinition
		newNamed: repositoryName 
		repositoryRoot: repoRoot pathString
		version: projectVersionString asRwSemanticVersionNumber.

"validate"
	self assert: repositoryDefinition_1  name = repositoryDefinition_2 name.
	self assert: repositoryDefinition_1  repositoryRoot = repositoryDefinition_2 repositoryRoot.
	self assert: repositoryDefinition_1  version = repositoryDefinition_2 version.

	self assert:  repositoryDefinition_1 version = projectVersionString asRwSemanticVersionNumber.
	self assert:  repositoryDefinition_1 repositoryRoot = repoRoot asFileReference.

"create repository definitions using #newNamed:repositoryRoot:version:"
	repositoryDefinition_1 := RwAbstractRepositoryDefinition
		newNamed: repositoryName 
		repositoryRoot: repoRoot
		projectUrl: gitHubProjectUrl
		version: projectVersionString.

	repositoryDefinition_2 := RwAbstractRepositoryDefinition
		newNamed: repositoryName 
		repositoryRoot: repoRoot pathString
		projectUrl: gitHubProjectUrl
		version: projectVersionString asRwSemanticVersionNumber.

"validate"
	self assert: repositoryDefinition_1  name = repositoryDefinition_2 name.
	self assert: repositoryDefinition_1  repositoryRoot = repositoryDefinition_2 repositoryRoot.
	self assert: repositoryDefinition_1  version = repositoryDefinition_2 version.
	self assert: repositoryDefinition_1  projectUrl = repositoryDefinition_2 projectUrl.

	self assert:  repositoryDefinition_1 projectUrl = gitHubProjectUrl.
]

{ #category : 'tests' }
RwRowanSample7Test >> testCreateRepositoryDefinitionFromUrl_1 [

	"exercise the RwAbstractRepositoryDefinition class creation protocol"

	| specUrlString rowanSpec gitRootPath projectName projectSpec repositoryDefinition_1
		repositoryDefinition_2 repositoryDefinition_3 repositoryDefinition_4 |

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

	repositoryDefinition_2 := RwAbstractRepositoryDefinition 
		newForSpecification: projectSpec 
			projectNamed: projectName
			projectHome: gitRootPath.

	repositoryDefinition_3 := RwAbstractRepositoryDefinition 
		newNamed: projectName 
			forSpecification: projectSpec 
			projectHome: gitRootPath.

	repositoryDefinition_4 := RwAbstractRepositoryDefinition 
		newNamed: projectName 
			forSpecification: projectSpec
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
	repositoryDefinition := RwAbstractRepositoryDefinition 
		newForSpecification: projectSpec 
			projectHome: gitRootPath.

"validate"
	self assert: (x := repositoryDefinition srcPath pathString) = (y := (gitRootPath / projectName / projectSpec repoPath) pathString).

"create repository definition based on memory"
	memoryRoot := FileSystem memory * 'repoDefinitionTestRoot'.
	self _repositoryRoot: memoryRoot.

	gitRootPath := memoryRoot / 'repos'.
	repositoryDefinition repositoryRoot: gitRootPath / projectName.

"validate"
	self assert: (x := repositoryDefinition srcPath pathString) = (y := (gitRootPath / projectName / projectSpec repoPath) pathString).

"create from scratch and it should be using RwMemoryRepositoryDefinition"
	repositoryDefinition_1 := RwAbstractRepositoryDefinition 
		newForSpecification: projectSpec 
			projectHome: gitRootPath.

"validate"
	self assert: repositoryDefinition  repositoryRoot fileSystem isMemoryFileSystem.

	self assert: repositoryDefinition  configsPath = repositoryDefinition_1 configsPath.
	self assert: repositoryDefinition  name = repositoryDefinition_1 name.
	self assert: repositoryDefinition  repositoryRoot = repositoryDefinition_1 repositoryRoot.
	self assert: repositoryDefinition  specsPath = repositoryDefinition_1 specsPath.
	self assert: repositoryDefinition  srcPath = repositoryDefinition_1 srcPath
]

{ #category : 'tests' }
RwRowanSample7Test >> testResolveProjectReference [

	"clone a repository from github, attach to an existing git repository, clone to an alternate projectHome"

	| specUrlString rowanSpec gitRootPath projectName projectSpec projectReferenceDefinition_1 projectReferenceDefinition_2 informHappened |

	projectName := 'RowanSample7'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	specUrlString := self _rowanSample7SpecificationUrl.
	projectSpec := specUrlString asRwUrl asSpecification.

"create project reference definitions"
	projectReferenceDefinition_1 := RwAbstractProjectReferenceDefinition 
		newForSpecification: projectSpec 
		projectHome: gitRootPath.

	projectReferenceDefinition_1 repositoryRoot ensureDeleteAll.

"1. clone repository"
	self 
		handleInformDuring: [ projectReferenceDefinition_1 resolve ] 
		interactionBlock: [:inform | self assert: false description: 'unexpected inform' ].

"validate"
	self assert: projectReferenceDefinition_1 repositoryRoot exists.

"2. create second project reference definitions"
	projectReferenceDefinition_2 := RwAbstractProjectReferenceDefinition 
		newForSpecification: projectSpec 
		projectHome: gitRootPath.

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
	self assert: projectReferenceDefinition_1 repositoryUrlScheme = projectReferenceDefinition_2 repositoryUrlScheme.

"3. now clone to a different spot by just changing the project home"
projectReferenceDefinition_2 repositoryRoot ensureDeleteAll.

	gitRootPath := gitRootPath asFileReference / 'sample7_repos'.
	gitRootPath ensureDeleteAll.
	gitRootPath ensureCreateDirectory.

	projectReferenceDefinition_2 projectHome: gitRootPath.

"clone to new location"
	self 
		handleInformDuring: [ projectReferenceDefinition_2 resolve ] 
		interactionBlock: [:inform | self assert: false dexcription: 'unexpected inform: ', inform message printString ].

"validate"
	self assert: projectReferenceDefinition_2 repositoryRoot exists.
	self deny: projectReferenceDefinition_1 repositoryRoot exists	"confirm that the repo wasn't cloned to old location"
]
