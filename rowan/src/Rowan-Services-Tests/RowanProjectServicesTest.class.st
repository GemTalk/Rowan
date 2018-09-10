Class {
	#name : 'RowanProjectServicesTest',
	#superclass : 'RowanServicesTest',
	#category : 'Rowan-Services-Tests'
}

{ #category : 'support' }
RowanProjectServicesTest >> existingProjectNamed: projectName [

	| projectService |
	projectService := RowanProjectService newNamed: projectName. 
	self assert: (Rowan image loadedProjectNamed: projectName) name equals: projectName.
	^projectService
]

{ #category : 'tests' }
RowanProjectServicesTest >> test_addPackage [

	| projectService projectName packageName loadedPackage |
	projectName := 'RowanSample1'. 
	packageName := 'TestAddPackage'.
	projectService := self existingProjectNamed: projectName.

	projectService addPackageNamed: packageName.
	loadedPackage := Rowan image loadedPackageNamed: packageName.
	self assert: loadedPackage name equals: packageName. 
	self assert: loadedPackage projectName equals: projectName.
]

{ #category : 'tests' }
RowanProjectServicesTest >> test_commandResultSessionTemp [

	self assert: RowanCommandResult results == (SessionTemps current at: #rowanCommandResults)
]

{ #category : 'tests' }
RowanProjectServicesTest >> test_updateAddsCommandResult [
	| projectName packageName projectService |
	projectName := 'RowanSample1'. 
	projectService := self existingProjectNamed: projectName.
	RowanCommandResult initializeResults. 
	self assert: RowanCommandResult results size equals: 0. 
	projectService update. 
	self assert: RowanCommandResult results size equals: 1. 
	self assert: RowanCommandResult results first == projectService.
]
