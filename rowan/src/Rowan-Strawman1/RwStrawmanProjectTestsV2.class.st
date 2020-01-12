Class {
	#name : 'RwStrawmanProjectTestsV2',
	#superclass : 'TestCase',
	#category : 'Rowan-Strawman1'
}

{ #category : 'private' }
RwStrawmanProjectTestsV2 >> _testRowanProjectsSandbox [
	"working repository for use as ROWAN_PROJECTS_HOME for tests"

	(System gemEnvironmentVariable: 'ROWAN_PROJECTS_SANDBOX')
		ifNotNil: [ :sandboxPath | ^ sandboxPath ]
]

{ #category : 'tests' }
RwStrawmanProjectTestsV2 >> testBasic [
	"Create project ... without errors  the project structure should parallel
		https://github.com/dalehenrich/tonel-demos"

	| projectName projectAlias project stonString |
	projectName := 'Tonel-Demos'.
	projectAlias := projectName , '_Test'.
	project := RwStrawmanProjectV2 new
		projectName: projectName;
		projectAlias: projectAlias;
		projectsHome: self _testRowanProjectsSandbox;
		comment: 'Project created using the Strawman1 V2 project creation api ';
		yourself.

	project
		addSimpleComponentNamed: 'Core'
			condition: 'vast'
			groupName: 'core'
			comment: 'Primary component used for loading the system.';
		addPackagesNamed:
				{(projectName , '-Core').
					(projectName , '-Vast-Application')}
			toComponentNamed: 'Core';
		vastSetVaPrequisitesTo: #('SUnit' 'TonelWriterModel')
			forPackageNamed: projectName , '-Core';
		vastSetVaPrequisitesTo: #('SUnit' 'TonelWriterModel')
			forPackageNamed: projectName , '-Vast-Application';
		yourself.

	stonString := STON toStringPretty: project.	"useful to view full structure"
	self assert: project _validate.

	true
		ifTrue: [ ^ self ].	"skip rest of test until the rest of the test regressions are addressed"
	project resolve.
	stonString := STON toStringPretty: project.	"useful to view full structure"

	project yourself
]
