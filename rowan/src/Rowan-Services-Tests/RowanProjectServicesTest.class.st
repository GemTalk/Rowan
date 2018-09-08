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
