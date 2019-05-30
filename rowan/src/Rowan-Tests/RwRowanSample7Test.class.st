Class {
	#name : 'RwRowanSample7Test',
	#superclass : 'RwToolTest',
	#instVars : [
		'repositoryRoot'
	],
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwRowanSample7Test >> _expected_rowanSample7_component_configuration [

	^ 'RwComponentLoadConfiguration {
	#name : ''Main'',
	#comment : '''',
	#version : ''0.1.0'',
	#conditionalPackages : {
		[
			''common''
		] : {
			''core'' : {
				#packageNames : [
					''RowanSample7_component-Core'',
					''RowanSample7_component-Extension''
				]
			},
			''tests'' : {
				#packageNames : [
					''RowanSample7_component-Tests_Main''
				]
			}
		},
		[
			''gemstone''
		] : {
			''core'' : {
				#packageNames : [
					''RowanSample7_component-GemStone-Core''
				]
			},
			''tests'' : {
				#packageNames : [
					''RowanSample7_component-Tests-GemStone_Main''
				]
			}
		}
	}
}'
]

{ #category : 'private' }
RwRowanSample7Test >> _expected_rowanSample7_component_specification [

^ 'RwComponentSpecification {
	#specName : ''RowanSample7_component'',
	#version : ''0.3.0'',
	#projectUrl : ''https://github.com/dalehenrich/RowanSample7'',
	#configsPath : ''rowan/components'',
	#specsPath : ''rowan/specs'',
	#repoSpec : RwGitRepositorySpecification {
		#committish : ''master'',
		#committishType : ''branch'',
		#remoteUrl : ''git@github.com:dalehenrich/RowanSample7.git ''
	},
	#repoPath : ''rowan/src'',
	#comment : ''test project reference'',
	#defaultConfigurationNames : [
		''Main''
	],
	#defaultGroupNames : [
		''core'',
		''tests''
	],
	#projectsPath : ''rowan/projects'',
	#defaultComponentName : ''Core''
}'
]

{ #category : 'private' }
RwRowanSample7Test >> _expected_rowanSample7_project_st [

^ '{ 
	#format : ''tonel'',
	#convention : ''RowanHybrid''
}
'
]

{ #category : 'private' }
RwRowanSample7Test >> _gitHubProjectUrl [

	^ 'https://github.com/dalehenrich/RowanSample7'
]

{ #category : 'private' }
RwRowanSample7Test >> _gitHubRemoteUrl [

	^ 'git@github.com:dalehenrich/RowanSample7.git'
]

{ #category : 'private' }
RwRowanSample7Test >> _projectVersionString [

	^ '0.0.3'
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
RwRowanSample7Test >> _rowanSample7_ColorsSpecificationUrl [

	| rowanSpec |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	^ 'file:' , rowanSpec repositoryRootPath , '/test/specs/RowanSample7_Colors.ston'
]

{ #category : 'private' }
RwRowanSample7Test >> _rowanSample7SpecificationUrl [

	| rowanSpec |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	^ 'file:' , rowanSpec repositoryRootPath , '/test/specs/RowanSample7.ston'
]

{ #category : 'private' }
RwRowanSample7Test >> _rowanSample7SpecificationUrl_12x [

	"1.2.x compat project spec"

	| rowanSpec |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	^ 'file:' , rowanSpec repositoryRootPath , '/test/specs/RowanSample7_12x.ston'
]

{ #category : 'running' }
RwRowanSample7Test >> tearDown [

	self _repositoryRoot ifNotNil: [:root | root ensureDelete ].
	super tearDown
]

{ #category : 'tests' }
RwRowanSample7Test >> testCreateComponentProject [

	"Excercise RwComponentProjectDefinition instance creation protocol ... 
		create the project on disk -- this is the full deal"

	| projectUrl projectName configurationNames groupNames comment projectHome
		rowanSpec cpd x y packageName className testClassName|

	projectName := 'RowanSample7_component'.
	configurationNames := #( 'Main' ).
	groupNames := #( 'core'  'tests').
	projectUrl := self _gitHubProjectUrl.
	comment := 'test project reference'.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].
	(Rowan image projectRepositoryNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :repo | Rowan image _removeProjectRepository: repo ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	projectHome := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

"create project definition"
	cpd := RwComponentProjectDefinition
		projectName: projectName 
			configurationNames: configurationNames 
			groupNames: groupNames 
			useGit: true 
			projectUrl: projectUrl 
			comment: comment.

"create package definitions"
	cpd
		addPackageNamed: projectName, '-Extension' 
			toComponentNamed: 'Main' 
			withConditions: #( 'common' ) 
			andGroupName: 'core';
		addPackageNamed: projectName, '-Core' 
			toComponentNamed: 'Main' 
			withConditions: #( 'common' ) 
			andGroupName: 'core';
		addPackageNamed: projectName, '-GemStone-Core' 
			toComponentNamed: 'Main' 
			withConditions: #( 'gemstone' ) 
			andGroupName: 'core';
		addPackageNamed: projectName, '-Tests_Main' 
			toComponentNamed: 'Main' 
			withConditions: #( 'common' ) 
			andGroupName: 'tests';
		addPackageNamed: projectName, '-Tests-GemStone_Main' 
			toComponentNamed: 'Main' 
			withConditions: #( 'gemstone' ) 
			andGroupName: 'tests';
		yourself.

"create class and method definitions"
	packageName := projectName, '-Core'.
	className := projectName, 'Core'.
	((cpd packageNamed: packageName)
		addClassNamed: className
			super: 'Object' 
			category: packageName)
		addInstanceMethod: 'foo ^''self foo_platform''' protocol: '*', packageName asLowercase.

	packageName := projectName, '-Extension'.
	((cpd packageNamed: packageName) addClassExtensionNamed: className)
		addClassMethod: 'foo ^''foo''' protocol: '*', packageName asLowercase.

	packageName := projectName, '-GemStone-Core'.
	((cpd packageNamed: packageName) addClassExtensionNamed: className)
		addInstanceMethod: 'foo_platform ^''foo_gemstone''' protocol: '*', packageName asLowercase.

	packageName := projectName, '-Tests_Main'.
	testClassName := projectName, 'Tests_Main'.
	((cpd packageNamed: packageName)
		addClassNamed: testClassName
			super: 'TestCase' 
			category: packageName)
		addInstanceMethod: 'test1 self assert: ', className, ' new foo = self _platform_response' protocol: '*', packageName asLowercase.

	packageName := projectName, '-Tests-GemStone_Main'.
	((cpd packageNamed: packageName) addClassExtensionNamed:testClassName)
		addInstanceMethod: '_platform_response ^ ''foo_gemstone''' protocol: '*', packageName asLowercase.

"prepare to export component project definition"
	cpd projectHome: projectHome.
	cpd repositoryRoot ensureDeleteAll.

"create component project on disk"
	cpd create.

"validate"
	self assert: cpd repositoryRoot exists.

	self assert: cpd configsRoot exists.
	self assert: (cpd configsRoot / (configurationNames at: 1) , 'ston') exists.
	self assert: (x := (cpd configsRoot / (configurationNames at: 1) , 'ston') readStream contents) = ( y:= self _expected_rowanSample7_component_configuration).

	self assert: cpd specsRoot exists.
	self assert: (cpd specsRoot / projectName , 'ston') exists.
	self assert: (x := (cpd specsRoot / projectName , 'ston') readStream contents) = ( y:= self _expected_rowanSample7_component_specification).

	self assert: cpd packagesRoot exists.
	self assert: (cpd packagesRoot /  'properties' , 'st') exists.
	self assert: (x := (cpd packagesRoot /  'properties' , 'st') readStream contents) = ( y:= self _expected_rowanSample7_project_st).
	{ projectName, '-GemStone-Core'. projectName, '-Core' . projectName, '-Tests-GemStone_Main'. 
		projectName, '-Extension' . projectName, '-Tests_Main' . projectName, '-Tests-GemStone_Main'
	}
		do: [:pName | 
			| packageDir |
			packageDir := cpd packageFormat = 'tonel'
				ifTrue: [ cpd packagesRoot / pName ]
				ifFalse: [ cpd packagesRoot / pName , 'package' ].
			self assert: packageDir exists ].

	self assert: cpd projectsRoot exists.

	self assert: (Rowan image loadedProjectNamed: projectName ifAbsent: [  ]) notNil.
	self assert: (Rowan image projectRepositoryNamed: projectName ifAbsent: [  ]) notNil
]

{ #category : 'tests' }
RwRowanSample7Test >> testCreateNewProjectFromUrl [

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
]

{ #category : 'tests' }
RwRowanSample7Test >> testCreateProjectReference [

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
]

{ #category : 'tests' }
RwRowanSample7Test >> testCreateProjectReferenceFromScratch [

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
	projectReferenceDefinition := RwProjectReferenceDefinition 
		newForSpecification: projectSpec 
		projectHome: gitRootPath.

"validate"
	self assert: projectReferenceDefinition projectName = projectName.
	self assert: projectReferenceDefinition projectAlias = projectReferenceDefinition projectName.
	self assert: projectReferenceDefinition configurationNames = #( 'Main').
	self assert: projectReferenceDefinition groupNames = #( 'core').
	self assert: projectReferenceDefinition defaultComponentName = 'Main'.
	self assert: projectReferenceDefinition projectUrl = 'https://github.com/dalehenrich/RowanSample7'.

	self assert: (x := projectReferenceDefinition projectHome pathString) = (y := gitRootPath asFileReference pathString).

"create project reference definitions"
	projectReferenceDefinition := RwProjectReferenceDefinition newForSpecification: projectSpec.

"validate"
	self assert: (x := projectReferenceDefinition projectHome pathString) = (y := FileLocator rowanProjectsHome pathString).
]

{ #category : 'tests' }
RwRowanSample7Test >> testCreateRepositoryDefinition [

	"exercise the RwAbstractRepositoryDefinition class creation protocol"

	| rowanSpec gitRootPath repositoryName repositoryDefinition_1  repositoryDefinition_2 repoRoot
		projectVersionString gitHubProjectUrl gitHubRemoteUrl x y |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	repositoryName := 'RowanSample7'.
	repoRoot := gitRootPath asFileReference / repositoryName.
	projectVersionString := self _projectVersionString.
	gitHubProjectUrl := self _gitHubProjectUrl.
	gitHubRemoteUrl := self _gitHubRemoteUrl.

"create repository definitions using #newNamed:repositoryRoot:"
	repositoryDefinition_1 := RwDiskRepositoryDefinition
		newNamed: repositoryName 
		repositoryRoot: repoRoot.

	repositoryDefinition_2 := RwDiskRepositoryDefinition
		newNamed: repositoryName 
		repositoryRoot: repoRoot pathString.

"validate"
	self assert: repositoryDefinition_1  name = repositoryDefinition_2 name.
	self assert: repositoryDefinition_1  repositoryRoot = repositoryDefinition_2 repositoryRoot.
	self assert:  repositoryDefinition_1 repositoryRoot = repoRoot asFileReference.

"create repository definitions using #newNamed:repositoryRoot:projectUrl:"
	repositoryDefinition_1 := RwDiskRepositoryDefinition
		newNamed: repositoryName 
		repositoryRoot: repoRoot
		projectUrl: gitHubProjectUrl.

	repositoryDefinition_2 := RwDiskRepositoryDefinition
		newNamed: repositoryName 
		repositoryRoot: repoRoot pathString
		projectUrl: gitHubProjectUrl.

"validate"
	self assert: repositoryDefinition_1  name = repositoryDefinition_2 name.
	self assert: repositoryDefinition_1  repositoryRoot = repositoryDefinition_2 repositoryRoot.
	self assert: repositoryDefinition_1  projectUrl = repositoryDefinition_2 projectUrl.

"create repository definitions using #newNamed:repositoryRoot:version:"
	repositoryDefinition_1 := RwDiskRepositoryDefinition
		newNamed: repositoryName 
		repositoryRoot: repoRoot.

	repositoryDefinition_2 := RwDiskRepositoryDefinition
		newNamed: repositoryName 
		repositoryRoot: repoRoot pathString.

"validate"
	self assert: repositoryDefinition_1  name = repositoryDefinition_2 name.
	self assert: repositoryDefinition_1  repositoryRoot = repositoryDefinition_2 repositoryRoot.

	self assert:  repositoryDefinition_1 repositoryRoot = repoRoot asFileReference.

"create repository definitions using #newNamed:repositoryRoot:projectUrl:version:"
	repositoryDefinition_1 := RwGitRepositoryDefinition
		newNamed: repositoryName 
		repositoryRoot: repoRoot
		projectUrl: gitHubProjectUrl
		remoteUrl: gitHubRemoteUrl
		committish: 'master'
		committishType: 'branch'.

	repositoryDefinition_2 := RwGitRepositoryDefinition
		newNamed: repositoryName 
		repositoryRoot: repoRoot pathString
		projectUrl: gitHubProjectUrl
		remoteUrl: gitHubRemoteUrl
		committish: 'master'
		committishType: 'branch'.

"validate"
	self assert: repositoryDefinition_1  name = repositoryDefinition_2 name.
	self assert: repositoryDefinition_1  repositoryRoot = repositoryDefinition_2 repositoryRoot.
	self assert: (x := repositoryDefinition_1  committish) = (y := repositoryDefinition_2 committish).
	self assert: repositoryDefinition_1  projectUrl = repositoryDefinition_2 projectUrl.
	self assert: repositoryDefinition_1  remoteUrl = repositoryDefinition_2 remoteUrl.

	self assert:  repositoryDefinition_1 projectUrl = gitHubProjectUrl.
	self assert: repositoryDefinition_1 remoteUrl = gitHubRemoteUrl.
	self assert: repositoryDefinition_1 committish = 'master'.
]

{ #category : 'tests' }
RwRowanSample7Test >> testCreateRepositoryDefinitionFromUrl_1 [

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
	repositoryDefinition := RwDiskRepositoryDefinition 
		newForSpecification: projectSpec 
			projectHome: gitRootPath.

	self assert: repositoryDefinition  repositoryRoot fileSystem isDiskFileSystem.
	self assert: repositoryDefinition class == RwDiskRepositoryDefinition.

"validate"
	self assert: (x := repositoryDefinition repositoryRoot pathString) = (y := (gitRootPath / projectName) pathString).

"create repository definition based on memory"
	memoryRoot := FileSystem memory * 'repoDefinitionTestRoot'.
	self _repositoryRoot: memoryRoot.

	gitRootPath := memoryRoot / 'repos'.
	repositoryDefinition repositoryRoot: gitRootPath / projectName.

"validate"
	self assert: repositoryDefinition  repositoryRoot fileSystem isMemoryFileSystem.
	self assert: (x := repositoryDefinition repositoryRoot pathString) = (y := (gitRootPath / projectName) pathString).

"create from scratch and it should be using RwMemoryRepositoryDefinition"
	repositoryDefinition_1 := RwAbstractRepositoryDefinition 
		newForSpecification: projectSpec 
			projectHome: gitRootPath.

"validate"
	self assert: repositoryDefinition_1  repositoryRoot fileSystem isMemoryFileSystem.
	self assert: repositoryDefinition_1 class == RwMemoryRepositoryDefinition.

	self assert: repositoryDefinition  name = repositoryDefinition_1 name.
	self assert: repositoryDefinition  repositoryRoot = repositoryDefinition_1 repositoryRoot.
]

{ #category : 'tests' }
RwRowanSample7Test >> testResolveProjectReference [

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
]

{ #category : 'tests' }
RwRowanSample7Test >> testResolveRepositoryDefinition [

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
]
