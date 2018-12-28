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
RowanServicesTest >> defaultSymbolDictionaryName [

	^'ServicesTestDictionary'
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

	RowanProjectService new newGitProject: 'file:$ROWAN_PROJECTS_HOME/Rowan/samples/RowanSample1.ston' root: '$ROWAN_PROJECTS_HOME' useSsh: true. 
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

	RowanCommandResult initializeResults.
]

{ #category : 'setup teardown' }
RowanServicesTest >> tearDown [

	RowanCommandResult initializeResults.  "squash the gui updates"
]

{ #category : 'support' }
RowanServicesTest >> unloadServicesTestProject [

	Rowan image loadedProjectNamed: self servicesTestProjectName ifAbsent:[^self].
	Rowan projectTools delete deleteProjectNamed: self servicesTestProjectName
]
