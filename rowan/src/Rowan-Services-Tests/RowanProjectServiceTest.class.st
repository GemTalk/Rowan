Class {
	#name : 'RowanProjectServiceTest',
	#superclass : 'RowanServicesTest',
	#category : 'Rowan-Services-Tests'
}

{ #category : 'support' }
RowanProjectServiceTest >> existingProjectNamed: projectName [

	| projectService |
	projectService := RowanProjectService newNamed: projectName. 
	self assert: (Rowan image loadedProjectNamed: projectName) name equals: projectName.
	^projectService
]

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
	self createServicesTestProject.
	self loadServicesTestProject.
]

{ #category : 'setup teardown' }
RowanProjectServiceTest >> tearDown [

	super tearDown.
	self unloadServicesTestProject
]

{ #category : 'tests' }
RowanProjectServiceTest >> test_addedProjectNotOnDisk [

	| proj projectService projectName |
	projectName := 'Tashkent'. 
	self jadeiteIssueTested: #issue246 withTitle: 'Jadeite handling project that''s not committed'. 
	proj := RowanTestService new createNewTestProjectNamed: projectName.
	projectService := RowanProjectService newNamed: projectName. 
	projectService refresh. "<-- walkback occured here" 
	[self deny: projectService existsOnDisk.
	self assert: projectService isSkew]
		ensure: [RowanBrowserService new unloadProjectsNamed: (Array with: 'Tashkent')]
]

{ #category : 'tests' }
RowanProjectServiceTest >> test_addPackage [

	| projectService  packageName loadedPackage |
	packageName := 'TestAddPackage'.
	projectService := self projectServiceNamed: self servicesTestProjectName.
	projectService addPackageNamed: packageName.
	loadedPackage := Rowan image loadedPackageNamed: packageName.
	self assert: loadedPackage name equals: packageName. 
	self assert: loadedPackage projectName equals: self servicesTestProjectName
]

{ #category : 'tests' }
RowanProjectServiceTest >> test_commandResultSessionTemp [

	self assert: RowanCommandResult results == (SessionTemps current at: #rowanCommandResults)
]

{ #category : 'tests' }
RowanProjectServiceTest >> test_updateAddsCommandResult [
	| projectService |
	projectService := self projectServiceNamed: self servicesTestProjectName.
	RowanCommandResult initializeResults. 
	self assert: RowanCommandResult results size equals: 0. 
	projectService update. 
	self assert: RowanCommandResult results size equals: 1. 
	self assert: RowanCommandResult results first == projectService.
]
