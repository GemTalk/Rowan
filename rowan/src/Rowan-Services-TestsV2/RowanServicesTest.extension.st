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
RowanServicesTest >> loadRowanSample1 [
  | gitRoot projectName spec projectTools |
  projectName := 'RowanSample1'.
  (Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
    ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].
  gitRoot := self _testRowanProjectsSandbox.
  (gitRoot / projectName) ensureDeleteAll.
	spec := 'file:$ROWAN_PROJECTS_HOME/Rowan/samples/RowanSample1.ston' asRwUrl asSpecification.
	projectTools := Rowan projectTools.
	projectTools clone
		cloneSpecification: spec
		gitRootPath: gitRoot pathString
		useSsh: true
		registerProject: false.	"does not register the project, so it is not visible in project list ... does however clone the project to local disk"
	"attach a project definition to the Rowan project on disk ... not loaded and not registered"
	projectTools create createProjectFromSpecUrl: 'file:', gitRoot pathString, '/', projectName, '/', spec specsPath, '/RowanSample1.ston'.
	projectTools load loadProjectNamed: 'RowanSample1'.
]

{ #category : '*rowan-services-testsv2' }
RowanServicesTest >> loadServicesTestProject [
	self defaultProjectDefinition load
]

{ #category : '*rowan-services-testsv2' }
RowanServicesTest >> servicesTestComponentName [

	^'Core'
]
