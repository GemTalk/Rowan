"
Set of tests that can be compiled and run as a non-SystemUser user. That means all operations, including the load (see platforms/gemstone/topaz/rowanDataCuratorTestSuite.gs) of the project must be accomplished by referencing only the Published/Globals Rowan classes.
"
Class {
	#name : 'RwDataCuratorTest',
	#superclass : 'TestCase',
	#instVars : [
		'preserveRowanProjectsHome'
	],
	#category : 'Rowan-DataCurator-Tests'
}

{ #category : 'private' }
RwDataCuratorTest >> _testRowanProjectsSandbox [
	"working repository for use as ROWAN_PROJECTS_HOME for tests"

	| rowanProject |
	(System gemEnvironmentVariable: 'ROWAN_PROJECTS_SANDBOX')
		ifNotNil: [ :sandboxPath | ^ sandboxPath asFileReference ].
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ rowanProject repositoryRoot / 'test/testRepositories/repos/'
]

{ #category : 'running' }
RwDataCuratorTest >> setUp [
	| projectsHome |
	super setUp.
	projectsHome := self _testRowanProjectsSandbox.
	preserveRowanProjectsHome := System gemEnvironmentVariable: 'ROWAN_PROJECTS_HOME'.
	System gemEnvironmentVariable: 'ROWAN_PROJECTS_HOME' put: projectsHome pathString.
]

{ #category : 'running' }
RwDataCuratorTest >> tearDown [
	super tearDown.
	System gemEnvironmentVariable: 'ROWAN_PROJECTS_HOME' put: preserveRowanProjectsHome.
]

{ #category : 'tests' }
RwDataCuratorTest >> testCreateProjects [
	"https://github.com/GemTalk/Rowan/issues/510"

	| projectTools projectName projectDef packageName className class project audit classNames extendedClasses componentName |
	projectTools := Rowan projectTools.
	projectName := 'MySampleProject'.
	componentName := 'Core'.
	packageName := projectName , '-Core'.
	className := 'MySampleClass'.

	projectDef := RwResolvedProjectV2 new
		projectName: projectName;
		addLoadComponentNamed: componentName;
		yourself.
	projectDef resolveProject.


	((projectDef addPackageNamed: packageName toComponentNamed: componentName)
		addClassNamed: className
		super: 'Object'
		category: packageName)
		addInstanceMethod: 'foo ^ 1'
		protocol: 'accessing'.
	projectDef load.
	Rowan
		projectNamed: projectName
		ifAbsent: [ 
			self
				assert: false
				description: 'The project ' , projectName printString , ' should exist' ].
	project := Rowan projectNamed: projectName.
	class := Rowan globalNamed: className.
	self assert: class notNil.
	self assert: (class new perform: #'foo') == 1.
	self assert: (audit := project audit) isEmpty.
	classNames := project definedClasses asArray collect: [ :each | each name ].
	self assert: classNames = {(className asSymbol)}.
	extendedClasses := project extendedClasses asArray.
	self assert: extendedClasses = {}.
	project unload.
	Rowan
		projectNamed: projectName
		ifPresent: [ 
			self
				assert: false
				description: 'The project ' , projectName printString , ' should not exist' ]
]
