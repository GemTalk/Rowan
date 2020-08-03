Extension { #name : 'RowanProjectServiceTest' }

{ #category : '*rowan-services-testsv2' }
RowanProjectServiceTest >> test_addPackage [

	| projectService  packageName loadedPackage |
	packageName := 'TestAddPackage'.
	projectService := self projectServiceNamed: self servicesTestProjectName.
	projectService addPackageNamed: packageName toComponentNamed: self servicesTestComponentName.
	loadedPackage := Rowan image loadedPackageNamed: packageName.
	self assert: loadedPackage name equals: packageName. 
	self assert: loadedPackage projectName equals: self servicesTestProjectName
]
