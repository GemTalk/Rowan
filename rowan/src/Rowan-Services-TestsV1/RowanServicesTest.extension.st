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

