Class {
	#name : 'RwStrawmanProjectTestsV2',
	#superclass : 'TestCase',
	#category : 'Rowan-Strawman1'
}

{ #category : 'private' }
RwStrawmanProjectTestsV2 >> _expectedPackageDefinitions [
	^ Dictionary new
		at: 'TonelExampleApp'
			put:
				(Dictionary new
						at: #'name' put: 'TonelExampleApp';
						at: #'properties'
							put:
								(Dictionary new
										at: 'vaPrerequisites' put: #('SUnit' 'TonelWriterModel');
										at: 'vaSubApplications'
											put:
												{(Dictionary new
														at: #'condition' put: true;
														at: #'subapps' put: {'TonelExampleSubApp'};
														yourself)};
										yourself);
						at: #'definitions' put: Dictionary new;
						yourself);
		yourself
]

{ #category : 'private' }
RwStrawmanProjectTestsV2 >> _testRowanProjectsSandbox [
	"working repository for use as ROWAN_PROJECTS_HOME for tests"

	(System gemEnvironmentVariable: 'ROWAN_PROJECTS_SANDBOX')
		ifNotNil: [ :sandboxPath | ^ sandboxPath ]
]

{ #category : 'tests' }
RwStrawmanProjectTestsV2 >> testBasic [
	"Create project ... without errors  the project structure should parallel
		https://github.com/dalehenrich/tonel-demos ...
		reading and writing packages is beyond the scope of this strawman"

	| projectName project stonString loadSpec resolvedProject x y |
	projectName := 'tonel-demos'.
	project := RwStrawmanProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		comment: 'Project created using the Strawman1 V2 project creation api ';
		yourself.

	project repositoryRoot ensureDeleteAll.

"create project from scratch"
	project
		addSimpleComponentNamed: 'Core'
			condition: 'vast'
			groupName: 'core'
			comment: 'Primary component used for loading the system.';
		addPackagesNamed:
				{'TonelExampleApp'}
			toComponentNamed: 'Core';
		vastSetVaPrequisitesTo: #('SUnit' 'TonelWriterModel')
			forPackageNamed: 'TonelExampleApp';
		vastSetVaSubApplicationsTo: {(Dictionary new
				at: #'condition' put: true;
				at: #'subapps' put: {'TonelExampleSubApp'};
				yourself)}
			forPackageNamed: 'TonelExampleApp';
		yourself.

	stonString := STON toStringPretty: project.	"useful to view full structure"
	self assert: project _validate.

	project resolve.

"export project arififacts to disk"
	stonString := STON toStringPretty: project.	"useful to view full structure"
	project
		export;
		exportLoadSpecification.

"read load spec from disk"
	loadSpec := (RwSpecification
		fromUrl:
			'file:'
				,
					(project repositoryRoot / 'rowan' / 'specs' / projectName , 'ston') pathString)
		projectsHome: self _testRowanProjectsSandbox;
		yourself.

"read project from disk"
	resolvedProject := loadSpec resolveStrawman.

	stonString := STON
		toStringPretty:
			{resolvedProject.
			project}.	"useful to view full structure"

"repository and components should match"
	self assert: resolvedProject _projectRepository = project _projectRepository.

	self assert: (x := resolvedProject _loadComponents components) = (y := project _loadComponents components).

	self assert: (x := resolvedProject packages) = (y := self _expectedPackageDefinitions).
]
