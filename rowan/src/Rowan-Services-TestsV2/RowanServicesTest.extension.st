Extension { #name : 'RowanServicesTest' }

{ #category : '*rowan-services-testsv2' }
RowanServicesTest >> createNonDiskTestProjectNamed: projectName packageName: packageName [
	| project componentName |
	componentName := self servicesTestComponentName.
	project := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self defaultSymbolDictionaryName;
		yourself.
	project resolve.

	(project addSimpleComponentNamed: componentName comment: 'a test component')
		addPackageNamed: packageName toComponentNamed: componentName;
		yourself.

	project load.
	^ project
]

{ #category : '*rowan-services-testsv2' }
RowanServicesTest >> createProjectDefinitionNamed: projectName [
	| project |
	project := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self defaultSymbolDictionaryName;
		yourself.
	project resolve.

	project addSimpleComponentNamed: self servicesTestComponentName comment: 'a test component'.
	^ project
]

{ #category : '*rowan-services-testsv2' }
RowanServicesTest >> createServicesTestPackage [
  | projectDefinition |
  defaultProjectDefinition
    ifNotNil: [ :projectDef | ^ projectDef packageNamed: self servicesTestPackageName ].
  projectDefinition := self defaultProjectDefinition.
  projectDefinition addPackageNamed: self servicesTestPackageName toComponentNamed: self servicesTestComponentName.
  ^ projectDefinition packageNamed: self servicesTestPackageName
]

{ #category : '*rowan-services-testsv2' }
RowanServicesTest >> servicesTestComponentName [

	^'Core'
]
