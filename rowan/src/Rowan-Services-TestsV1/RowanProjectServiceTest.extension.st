Extension { #name : 'RowanProjectServiceTest' }

{ #category : '*rowan-services-testsv1' }
RowanProjectServiceTest >> test_addPackage [

	| projectService  packageName loadedPackage |
	packageName := 'TestAddPackage'.
	projectService := self projectServiceNamed: self servicesTestProjectName.
	projectService addPackageNamed: packageName.
	loadedPackage := Rowan image loadedPackageNamed: packageName.
	self assert: loadedPackage name equals: packageName. 
	self assert: loadedPackage projectName equals: self servicesTestProjectName
]
