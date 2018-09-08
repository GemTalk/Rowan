Class {
	#name : 'RowanProjectServicesTest',
	#superclass : 'RowanServicesTest',
	#category : 'Rowan-Services-Tests'
}

{ #category : 'tests' }
RowanProjectServicesTest >> test_addPackage [

	| projectService browserService projectName packageName loadedPackage |
	projectName := 'TestAddPackageProject'. 
	packageName := 'TestAddPackage'.
	projectService := RowanProjectService newNamed: projectName. 

	[
	projectService addPackageNamed: packageName.
	loadedPackage := Rowan image loadedPackageNamed: packageName.
	self assert: loadedPackage name equals: packageName. 
	self assert: loadedPackage rowanProjectName equals: projectName. 
	] ensure: [
		browserService := RowanBrowserService new. 
		browserService unloadProjectsNamed: (Array with: projectName)]
]
