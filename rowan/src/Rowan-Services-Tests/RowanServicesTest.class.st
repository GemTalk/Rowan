"
RowanServicesTest subclasses require RowanSample1 project to be loaded.
"
Class {
	#name : 'RowanServicesTest',
	#superclass : 'TestCase',
	#instVars : [
		'defaultProjectDefinition'
	],
	#category : 'Rowan-Services-Tests'
}

{ #category : 'unicode method' }
RowanServicesTest >> compileUnicodeMethod [
	"RowanServicesTest new compileUnicodeMethod"

	RowanServicesTest rwCompileMethod:
	'iAmAUnicodeMethod

		| abc |
		abc := ''', (String with: (Character withValue: 16r3DA)), '''.
		self halt. 
		^abc'

	category: 'unicode method'
]

{ #category : 'support' }
RowanServicesTest >> createClassDefinitionNamed: className [

	| classDefinition |
	classDefinition := RwClassDefinition
		newForClassNamed: className
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: 'services test'
			comment: String new
			pools: #()
			type: 'normal'.
	^classDefinition
]

{ #category : 'support' }
RowanServicesTest >> createJadeiteTestPackage [
	| proj pkg |
	proj := self createJadeiteTestProject. 
	pkg := RwPackageDefinition newNamed: self servicesTestPackageName.
	proj addPackage: pkg.
	Rowan projectTools load loadProjectDefinition: proj.
	^pkg.
]

{ #category : 'support' }
RowanServicesTest >> createJadeiteTestProject [
	| proj |
	proj := RwProjectDefinition newForGitBasedProjectNamed: self servicesTestProjectName. 
	proj repositoryRootPath: '$ROWAN_PROJECTS_HOME/', self servicesTestProjectName. 
	Rowan projectTools create createProjectFor: proj.
	^proj
]

{ #category : 'support' }
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

{ #category : 'support' }
RowanServicesTest >> createProjectDefinitionNamed: projectName [

	| projectDefinition |

	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		defaultSymbolDictName: self defaultSymbolDictionaryName;
		yourself.
	^projectDefinition
]

{ #category : 'support' }
RowanServicesTest >> createServicesTestClass [

	| packageDefinition classDefinition |
	packageDefinition := self createServicesTestPackage.
	classDefinition := self createClassDefinitionNamed: self servicesDefaultClassName. 
	packageDefinition addClassDefinition: classDefinition. 
	^classDefinition
]

{ #category : 'support' }
RowanServicesTest >> createServicesTestPackage [

	| projectDefinition |
	projectDefinition := self createServicesTestProject. 
	projectDefinition addPackageNamed: self servicesTestPackageName.
	^projectDefinition packageNamed: self servicesTestPackageName
]

{ #category : 'support' }
RowanServicesTest >> createServicesTestProject [

	defaultProjectDefinition := self createProjectDefinitionNamed: self servicesTestProjectName.
	^defaultProjectDefinition
]

{ #category : 'support' }
RowanServicesTest >> createServicesTestTestClass [

	| packageDefinition classDefinition |
	packageDefinition := defaultProjectDefinition packageNamed: self servicesTestPackageName. 
	classDefinition := self createTestClassDefinitionNamed: self servicesDefaultTestClassName. 
	packageDefinition addClassDefinition: classDefinition. 
	^classDefinition
]

{ #category : 'support' }
RowanServicesTest >> createTestClassDefinitionNamed: className [

	| classDefinition |
	classDefinition := RwClassDefinition
		newForClassNamed: className
			super: 'TestCase'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: 'services test'
			comment: String new
			pools: #()
			type: 'normal'.
	^classDefinition
]

{ #category : 'support' }
RowanServicesTest >> defaultSymbolDictionaryName [

	^'ServicesTestDictionary'
]

{ #category : 'unicode method' }
RowanServicesTest >> iAmAUnicodeMethod [

		| abc |
		abc := 'Ϛ'.
		self halt. 
		^abc
]

{ #category : 'support' }
RowanServicesTest >> jadeiteIssueTested: aSymbol withTitle: anObject [
	"send this to help identify which tests test which issues
	format: #issue<issueNumber>
	Example self issueTested: #issue235 withTitle: 'Need Find Class/Method from console'

	Issues currently reside in: 
		https://github.com/GemTalk/Jadeite/issues/"
]

{ #category : 'support' }
RowanServicesTest >> loadRowanSample1 [
	| specUrlString projectTools rowanSpec gitRootPath projectName projectDefinition spec |

	projectName := 'RowanSample1'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [])
		ifNotNil: [ :prj |  Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := 'file:' , rowanSpec repositoryRootPath , '/samples/RowanSample1.ston'.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	spec := specUrlString asRwUrl asSpecification.
	projectTools clone
		cloneSpecification: spec
		gitRootPath: gitRootPath
		useSsh: true
		registerProject: false.	"does not register the project, so it is not visible in project list ... does however clone the project to local disk"
	
	"attach a project definition to the Rowan project on disk ... not loaded and not registered"
	projectDefinition := projectTools create createProjectFromSpecUrl: 'file:', gitRootPath, '/', projectName, '/', spec specsPath, '/RowanSample1.ston'.
	Rowan projectTools load loadProjectNamed: 'RowanSample1'.
]

{ #category : 'support' }
RowanServicesTest >> loadServicesTestProject [
	
	| projectSetDefinition |

	projectSetDefinition:= RwProjectSetDefinition new.
	projectSetDefinition addDefinition: self servicesTestProjectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.
]

{ #category : 'constants' }
RowanServicesTest >> servicesDefaultClassName [

	^'RowanServicesTestClass'
]

{ #category : 'constants' }
RowanServicesTest >> servicesDefaultTestClassName [

	^'RowanServicesTestClassForTesting'
]

{ #category : 'constants' }
RowanServicesTest >> servicesTestPackageName [

	^'RowanServicesTestPackage'
]

{ #category : 'support' }
RowanServicesTest >> servicesTestProjectDefinition [

	^defaultProjectDefinition ifNil:[defaultProjectDefinition := self createServicesTestProject]
]

{ #category : 'constants' }
RowanServicesTest >> servicesTestProjectName [

	^'RowanServicesTestProject'
]

{ #category : 'setup teardown' }
RowanServicesTest >> setUp [
	"ensure results are clean as service requests not coming through #updateFromSton: like the client"
	RowanAnsweringService new setEnableInteractionHandler: false. 
	RowanCommandResult initializeResults.
]

{ #category : 'setup teardown' }
RowanServicesTest >> tearDown [

	RowanCommandResult initializeResults.  "squash the gui updates"
	RowanAnsweringService new setEnableInteractionHandler: true.
]

{ #category : 'support' }
RowanServicesTest >> unloadServicesTestProject [

	Rowan image loadedProjectNamed: self servicesTestProjectName ifAbsent:[^self].
	Rowan projectTools delete deleteProjectNamed: self servicesTestProjectName
]
