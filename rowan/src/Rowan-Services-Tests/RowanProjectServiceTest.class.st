Class {
	#name : 'RowanProjectServiceTest',
	#superclass : 'RowanServicesTest',
	#category : 'Rowan-Services-Tests'
}

{ #category : 'support' }
RowanProjectServiceTest >> projectServiceNamed: projectName [

	| projectService |
	projectService := RowanProjectService newNamed: projectName. 
	self assert: (Rowan image loadedProjectNamed: projectName) name equals: projectName.
	^projectService
]

{ #category : 'setup teardown' }
RowanProjectServiceTest >> setUp [

	super setUp.
	self createDefaultProject.
	self loadDefaultProject.
]

{ #category : 'setup teardown' }
RowanProjectServiceTest >> tearDown [

	super tearDown.
	self unloadDefaultProject
]

{ #category : 'tests' }
RowanProjectServiceTest >> test_addedProjectNotOnDisk [

	| proj projectService |
	self jadeiteIssueTested: #issue246 withTitle: 'Jadeite handling project that''s not committed'. 
	proj := RwProjectDefinition newForGitBasedProjectNamed: 'Tashkent'.
	proj repositoryRootPath: '$ROWAN_PROJECTS_HOME/Tashkent'.
	Rowan projectTools create createProjectFor: proj.
	Rowan projectTools load loadProjectDefinition: proj.
	projectService := RowanProjectService newNamed: 'Tashkent'. 
	projectService refresh. "<-- walkback occured here" 
	[self deny: projectService existsOnDisk.
	self assert: projectService isSkew]
		ensure: [RowanBrowserService new unloadProjectsNamed: (Array with: 'Tashkent')]
]

{ #category : 'tests' }
RowanProjectServiceTest >> test_addPackage [

	| projectService  packageName loadedPackage |
	packageName := 'TestAddPackage'.
	projectService := self projectServiceNamed: self defaultProjectName.
	projectService addPackageNamed: packageName.
	loadedPackage := Rowan image loadedPackageNamed: packageName.
	self assert: loadedPackage name equals: packageName. 
	self assert: loadedPackage projectName equals: self defaultProjectName
]

{ #category : 'tests' }
RowanProjectServiceTest >> test_commandResultSessionTemp [

	self assert: RowanCommandResult results == (SessionTemps current at: #rowanCommandResults)
]

{ #category : 'tests' }
RowanProjectServiceTest >> test_updateAddsCommandResult [
	| projectService |
	projectService := self projectServiceNamed: self defaultProjectName.
	RowanCommandResult initializeResults. 
	self assert: RowanCommandResult results size equals: 0. 
	projectService update. 
	self assert: RowanCommandResult results size equals: 1. 
	self assert: RowanCommandResult results first == projectService.
]
