Class {
	#name : 'RwProjectComponentDefinitionsTest',
	#superclass : 'RwAbstractTest',
	#category : 'Rowan-Tests-Components'
}

{ #category : 'private' }
RwProjectComponentDefinitionsTest class >> _symbolDictionaryNames [
	^ super _symbolDictionaryNames , #(#'SampleSymbolDict'  #'RowanSample2' #'RowanSample2_SD')
]

{ #category : 'tests' }
RwProjectComponentDefinitionsTest >> testCloneComponentProject [

	| rowanProject urlString projectDefinition |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	urlString :=  'file:' , rowanProject repositoryRootPath , '/test/specs/RowanSample1_masterV2.ston'.

	projectDefinition := (urlString asRwUrl asSpecification asDefinition)
		projectHome: self _testRowanProjectsSandbox;
		yourself.

"setup"
	projectDefinition repositoryRoot ensureDeleteAll.
	self deny: (projectDefinition repositoryRoot / 'rowan' / 'projects' / 'RowanSample2.ston') exists.

"clone"
	projectDefinition clone.

"validate"
	self assert: (projectDefinition repositoryRoot / 'rowan' / 'projects' / 'RowanSample2.ston') exists.	"present in masterV2.0 branch"
]

{ #category : 'tests' }
RwProjectComponentDefinitionsTest >> testCreateComponentProject_01 [

	"create Rowan project from scratch"

	"execute without errors .. for now"

	| projectName project |
	projectName := 'Example'.
	Rowan projectNamed: projectName ifPresent: [:prj | prj unload ].

	project := RwComponentProjectDefinition
		projectName: projectName 
			componentNamesToLoad: #( 'Core' )
			groupNamesToLoad: #( 'core' 'tests' )
			defaultComponentName: 'Core'
			defaultGroupName: 'core'
			packageFormat: 'tonel'
			projectHome: '$ROWAN_PROJECTS_HOME'
			specsPath: 'rowan/specs'
			componentsPath: 'rowan/componenets'
			packagesPath: 'rowan/src'
			projectsPath: 'rowan/projects'
			useGit: true
			comment: 'Example project exposing the standard project creation api'.
	project
		addComponentNamed: 'Core'
			definedGroupNames: 
				(Dictionary new
					add: 'core'  -> {};
					add: 'tests' -> { 'core' };
					yourself)
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'ExampleCore'
			comment: 'Primary component used for loading entire system.';
		addPackageNamed: 'Example-Core1'
			withConditions: { 'common' };
		addPackageNamed: 'Example-Core2'
			withConditions: { 'common' };
		addPackageNamed: 'Example-Tests'
			withConditions: { 'common' }
			andGroupName: 'tests';
		addPackageNamed: 'Example-Test-Extensions'
			toComponentNamed: 'Core'
			withConditions: { 'gemstone' }
			andGroupName: 'tests';
		addPackageNamed: 'Example-GemStone-Extensions'
			toComponentNamed: 'Core'
			withConditions: { 'gemstone' }
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'Globals';
		addPackageNamed: 'Example-GemStone-Test-Extensions'
			toComponentNamed: 'Core'
			withConditions: { 'gemstone' }
			andGroupName: 'tests'
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'Globals';
		yourself
]

{ #category : 'tests' }
RwProjectComponentDefinitionsTest >> testCreateComponentProject_02 [


	"Create a Rowan project from scratch, using the available defaults"

	"execute without errors .. for now"

	| projectName project |
	projectName := 'Example'.
	Rowan projectNamed: projectName ifPresent: [:prj | prj unload ].

	project := RwComponentProjectDefinition
		projectName: projectName 
			projectHome: '$ROWAN_PROJECTS_HOME'
			useGit: true
			comment: 'Example project exposing the standard project creation api'.
	project
		addComponentNamed: 'Core'
			comment: 'Primary component used for loading entire system.';
		addPackagesNamed: { 'Example-Core1' . 'Example-Core2' };
		addPackageNamed: 'Example-Core3'
			withConditions: { 'common' };
		addPackageNamed: 'Example-Core4'
			withConditions: { 'common' };
		addPackageNamed: 'Rowan-Tests'
			withConditions: { 'common' }
			andGroupName: 'tests';
		addPackageNamed: 'Example-GemStone-Extensions'
			withConditions: { 'gemstone' }
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'Globals';
		addPackageNamed: 'Example-GemStone-Test-Extensions'
			withConditions: { 'gemstone' }
			andGroupName: 'tests'
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'Globals';
		yourself
]

{ #category : 'tests' }
RwProjectComponentDefinitionsTest >> testCreateComponentProject_03 [

	"Create a Rowan project from scratch, using array-based apis for sets of packages that have the same
		package attributes"

	"execute without errors .. for now"

	| projectName project |
	projectName := 'Example'.
	Rowan projectNamed: projectName ifPresent: [:prj | prj unload ].

	project := RwComponentProjectDefinition
		projectName: projectName 
			componentNamesToLoad: #( 'Core' )
			groupNamesToLoad: #( 'core' 'tests' )
			defaultComponentName: 'Core'
			defaultGroupName: 'core'
			packageFormat: 'tonel'
			projectHome: '$ROWAN_PROJECTS_HOME'
			specsPath: 'rowan/specs'
			componentsPath: 'rowan/componenets'
			packagesPath: 'rowan/src'
			projectsPath: 'rowan/projects'
			useGit: true
			comment: 'Example project exposing the standard project creation api'.
	project
		addComponentNamed: 'Core'
			definedGroupNames: 
				(Dictionary new
					add: 'core'  -> {};
					add: 'tests' -> { 'core' };
					yourself)
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'ExampleCore'
			comment: 'Primary component used for loading entire system.';
		addPackagesNamed: {'Example-Core1' . 'Example-Core2'}
			withConditions: { 'common' };
		addPackagesNamed: { 'Example-Tests1' . 'Example-Tests2' }
			withConditions: { 'common' }
			andGroupName: 'tests';
		addPackagesNamed: { 'Example-Test-Extensions1' . 'Example-Test-Extensions2' }
			toComponentNamed: 'Core'
			withConditions: { 'common' }
			andGroupName: 'tests';
		addPackagesNamed: { 'Example-GemStone-Extensions1' . 'Example-GemStone-Extensions2' }
			toComponentNamed: 'Core'
			withConditions: { 'gemstone' }
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'Globals';
		addPackagesNamed: { 'Example-GemStone-Extensions3' . 'Example-GemStone-Extensions4' }
			withConditions: { 'gemstone' }
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'Globals';
		addPackagesNamed: { 'Example-GemStone-Test-Extensions1' . 'Example-GemStone-Test-Extensions2' }
			toComponentNamed: 'Core'
			withConditions: { 'gemstone' }
			andGroupName: 'tests'
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'Globals';
		addPackagesNamed: { 'Example-GemStone-Test-Extensions3' . 'Example-GemStone-Test-Extensions4' }
			withConditions: { 'gemstone' }
			andGroupName: 'tests'
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'Globals';
		yourself
]

{ #category : 'tests' }
RwProjectComponentDefinitionsTest >> testLoadRowanSample1_masterV20 [

	| rowanProject projectsHome urlString projectSample1Definition projectSample2Definition 
		projectSetDefinition1 projectSetDefinition2 x |

	#( 'RowanSample1' 'RowanSample2') do: [:prjName |
		(Rowan image loadedProjectNamed: prjName ifAbsent: [  ])
			ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	projectsHome := self self _testRowanProjectsSandbox.

	urlString :=  'file:' , rowanProject repositoryRootPath , '/test/specs/RowanSample1_masterV2.ston'.
	projectSample1Definition := (urlString asRwUrl asSpecification asDefinition)
		projectHome: projectsHome;
		yourself.

	urlString :=  'file:' , rowanProject repositoryRootPath , '/test/specs/RowanSample2_masterV2.ston'.
	projectSample2Definition := (urlString asRwUrl asSpecification asDefinition)
		projectHome: projectsHome;
		yourself.

"setup"
	projectSample1Definition repositoryRoot ensureDeleteAll.
	projectSample2Definition repositoryRoot ensureDeleteAll.

"clone"
	projectSetDefinition2 := projectSample2Definition clone.	"we want RowanSample2 to be freshly cloned into projectsHome"
	projectSetDefinition1 := projectSample1Definition clone.

"validate"
	self assert: (x := projectSetDefinition1 projectNames asArray sort) = #( 'RowanSample1' 'RowanSample2').

"load both 1 and 2"
	projectSample1Definition load.

"validate"
]

{ #category : 'tests' }
RwProjectComponentDefinitionsTest >> testReadComponentProject [

	| rowanProject urlString projectDefinition x projectSetDefinition |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	urlString :=  'file:' , rowanProject repositoryRootPath , '/test/specs/RowanSample2_masterV2.ston'.

	projectDefinition := (urlString asRwUrl asSpecification asDefinition)
		projectHome: self _testRowanProjectsSandbox;
		yourself.

"setup"
	projectDefinition repositoryRoot ensureDeleteAll.
	self assert: (x := projectDefinition packageNames isEmpty).

"clone"
	projectSetDefinition := projectDefinition clone.	"does a read as part of the clone"

"validate"
	self assert: (x := projectSetDefinition projectNames asArray sort) = #( 'RowanSample2').
	self assert: (x := projectDefinition packageNames asArray sort) = #( 'RowanSample2-Core')
]

{ #category : 'tests' }
RwProjectComponentDefinitionsTest >> testRowanInstall_3 [

	"derived from platforms/gemstone/topaz/3.5.0/install_3.tpz"

	"install_3.tpz"
 	| projectSetDefinition gitRepoPath  loadedProjectInfo readTool gitRoot x |
	projectSetDefinition := RwProjectSetDefinition new.
	loadedProjectInfo := Dictionary new.
	gitRepoPath := '$ROWAN_PROJECTS_HOME/Rowan'.
	readTool := Rowan projectTools read.
	{	{
			'file:$ROWAN_PROJECTS_HOME/Rowan/rowan/specs/RowanV2.ston'. 
			'$ROWAN_PROJECTS_HOME'
		}	} 
	do: [:ar |
		"Read project and packages from disk, creating a projectSetDefinition with all 5 projects"
		| projectDefinition theProjectSetDefinition specUrl projectHome |
		specUrl := ar at: 1.
		projectHome := ar at: 2.
		gitRoot := '$ROWAN_PROJECTS_HOME/Rowan'.
		projectDefinition := (RwComponentProjectDefinition newForUrl: specUrl) 
			projectHome: projectHome;
			gitRoot: gitRoot;
			yourself.
		theProjectSetDefinition := readTool 
			readProjectSetForComponentProjectDefinition: projectDefinition 
				withConfigurations: projectDefinition defaultConfigurationNames 
				groupNames: projectDefinition defaultGroupNames.
		self assert: #( 'Load' ) sort = (x := (theProjectSetDefinition projectNamed: projectDefinition name) loadedConfigurationNames asArray sort) ]
]
