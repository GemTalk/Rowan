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

{ #category : 'testing' }
RowanServicesTest class >> isAbstract [
  ^ self sunitName = #'RowanServicesTest'
]

{ #category : 'unicode method' }
RowanServicesTest >> addUnicodeSymbolKeyToUserGlobals [
  "RowanServicesTest new addUnicodeSymbolKeyToUserGlobals"

  UserGlobals
    at: (String with: (Character withValue: 16r3DA)) asSymbol
    put: 'fnoodle'
]

{ #category : 'unicode method' }
RowanServicesTest >> addUnicodeValueToUserGlobals [
  "RowanServicesTest new addUnicodeValueToUserGlobals"

  UserGlobals at: #'bad' put: '£¥' asSymbol
]

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
  packageDefinition classDefinitions
    do: [ :classDef | 
      classDef key = self servicesDefaultClassName
        ifTrue: [ ^ classDef ] ].
  classDefinition := self
    createClassDefinitionNamed: self servicesDefaultClassName.
  packageDefinition addClassDefinition: classDefinition.
  ^ classDefinition
]

{ #category : 'support' }
RowanServicesTest >> createServicesTestPackage [
  | projectDefinition |
  defaultProjectDefinition
    ifNotNil: [ :projectDef | ^ projectDef packageNamed: self servicesTestPackageName ].
  projectDefinition := self defaultProjectDefinition.
  projectDefinition addPackageNamed: self servicesTestPackageName.
  ^ projectDefinition packageNamed: self servicesTestPackageName
]

{ #category : 'support' }
RowanServicesTest >> createServicesTestTestClass [
  | packageDefinition classDefinition |
  packageDefinition := self defaultProjectDefinition
    packageNamed: self servicesTestPackageName.
  classDefinition := self
    createTestClassDefinitionNamed: self servicesDefaultTestClassName.
  packageDefinition addClassDefinition: classDefinition.
  ^ classDefinition
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
RowanServicesTest >> defaultProjectDefinition [
  ^ defaultProjectDefinition
    ifNil: [ 
      defaultProjectDefinition := self
        createProjectDefinitionNamed: self servicesTestProjectName ]
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
  | rowanSpec gitRootPath projectName  |
  projectName := 'RowanSample1'.
  (Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
    ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].
  rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
  gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.
  (Rowan fileUtilities directoryExists: gitRootPath , projectName)
    ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].
  Rowan projectTools clone
    cloneSpecUrl: 'file:$ROWAN_PROJECTS_HOME/Rowan/samples/RowanSample1.ston'
    gitRootPath: gitRootPath
    useSsh: false.
  Rowan projectTools load loadProjectNamed: projectName
]

{ #category : 'support' }
RowanServicesTest >> loadServicesTestProject [
  | projectSetDefinition |
  projectSetDefinition := RwProjectSetDefinition new.
  projectSetDefinition addDefinition: self defaultProjectDefinition.
  Rowan projectTools load loadProjectSetDefinition: projectSetDefinition
]

{ #category : 'unicode method' }
RowanServicesTest >> removeUnicodeSymbolsFromUserGlobals [
  "RowanServicesTest new addUnicodeSymbolToUserGlobals"

  UserGlobals
    removeKey: (String with: (Character withValue: 16r3DA)) asSymbol
    ifAbsent: [  ].
  UserGlobals removeKey: #'bad' ifAbsent: [  ]
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

{ #category : 'constants' }
RowanServicesTest >> servicesTestProjectName [

	^'RowanServicesTestProject'
]

{ #category : 'setup teardown' }
RowanServicesTest >> setUp [
  "ensure results are clean as service requests not coming through #updateFromSton: like the client"

  super setUp.
  SessionTemps current at: #'versionsVerified' put: false.
  RowanAnsweringService new setEnableInteractionHandler: false.
  RowanCommandResult initializeResults.
  Rowan platform _alternateImageClass: Rowan image testImageClass
]

{ #category : 'setup teardown' }
RowanServicesTest >> tearDown [
  super tearDown.
  Rowan platform _alternateImageClass: nil.
  RowanCommandResult initializeResults.	"squash the gui updates"
  RowanAnsweringService new setEnableInteractionHandler: true
]

{ #category : 'support' }
RowanServicesTest >> unloadServicesTestProject [

	Rowan image loadedProjectNamed: self servicesTestProjectName ifAbsent:[^self].
	Rowan projectTools delete deleteProjectNamed: self servicesTestProjectName.
	defaultProjectDefinition := nil
]
