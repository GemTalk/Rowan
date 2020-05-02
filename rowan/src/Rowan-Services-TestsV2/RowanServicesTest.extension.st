Extension { #name : 'RowanServicesTest' }

{ #category : '*rowan-services-testsv2' }
RowanServicesTest >> createNonDiskTestProjectNamed: projectName packageName: packageName [
	| project componentName |
	Rowan image
		newOrExistingSymbolDictionaryNamed: self defaultSymbolDictionaryName.	"make sure symbol dictionary is created at same time project is created"
	componentName := self servicesTestComponentName.
	project := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self defaultSymbolDictionaryName;
		repoType: #none;
		yourself.
	project resolve.

	project
		addSimpleComponentNamed: componentName comment: 'a test component';
		addPackageNamed: packageName toComponentNamed: componentName;
		gemstoneSetSymbolDictName: self defaultSymbolDictionaryName
			forPackageNamed: packageName;
		yourself.

	project load.
	^ project
]

{ #category : '*rowan-services-testsv2' }
RowanServicesTest >> createProjectDefinitionNamed: projectName [
	| project |
	Rowan image
		newOrExistingSymbolDictionaryNamed: self defaultSymbolDictionaryName.	"make sure symbol dictionary is created at same time project is created"
	project := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self defaultSymbolDictionaryName;
		yourself.
	project resolve.

	project
		addSimpleComponentNamed: self servicesTestComponentName
		comment: 'a test component'.

	project load.
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
RowanServicesTest >> loadRowanSample1 [
	| projectsHome projectName loadSpec resolvedProject |
	projectName := 'RowanSample1'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].
	projectsHome := self _testRowanProjectsSandbox.
	(projectsHome / projectName) ensureDeleteAll.
	loadSpec := (RwSpecification
		fromUrl:
			'file:$ROWAN_PROJECTS_HOME/Rowan/samples/RowanSample1_resolved_v2.ston')
		projectsHome: projectsHome;
		yourself.
	resolvedProject := loadSpec resolve.
	resolvedProject load
]

{ #category : '*rowan-services-testsv2' }
RowanServicesTest >> loadServicesTestProject [
	self defaultProjectDefinition load
]

{ #category : '*rowan-services-testsv2' }
RowanServicesTest >> servicesTestComponentName [

	^'Core'
]
