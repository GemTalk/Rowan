Extension { #name : 'RowanServicesTest' }

{ #category : '*rowan-services-testsv1' }
RowanServicesTest >> createNonDiskTestProjectNamed: projectName packageName: packageName [

	| projectDefinition projectSetDefinition  |
	projectDefinition := RwProjectDefinition newForGitBasedProjectNamed: projectName.
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		yourself.
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.
	^projectDefinition
]

{ #category : '*rowan-services-testsv1' }
RowanServicesTest >> createProjectDefinitionNamed: projectName [

	| projectDefinition |

	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		defaultSymbolDictName: self defaultSymbolDictionaryName;
		yourself.
	^projectDefinition
]

{ #category : '*rowan-services-testsv1' }
RowanServicesTest >> createServicesTestPackage [
  | projectDefinition |
self error: 'needs porting back to v1 api'.
  defaultProjectDefinition
    ifNotNil: [ :projectDef | ^ projectDef packageNamed: self servicesTestPackageName ].
  projectDefinition := self defaultProjectDefinition.
  projectDefinition addPackageNamed: self servicesTestPackageName toComponentNamed: self servicesTestComponentName.
  ^ projectDefinition packageNamed: self servicesTestPackageName
]

{ #category : '*rowan-services-testsv1' }
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
  | projectSetDefinition |
  projectSetDefinition := RwProjectSetDefinition new.
  projectSetDefinition addDefinition: self defaultProjectDefinition.
  Rowan projectTools load loadProjectSetDefinition: projectSetDefinition
]


