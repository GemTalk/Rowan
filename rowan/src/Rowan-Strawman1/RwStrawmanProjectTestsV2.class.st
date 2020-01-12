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
		https://github.com/dalehenrich/tonel-demos ...
		reading and writing packages is beyond the scope of this strawman"

	| projectName projectAlias project stonString loadSpec resolvedProject |
	projectName := 'Tonel-Demos'.
	projectAlias := projectName , '_Test'.
	project := RwStrawmanProjectV2 new
		projectName: projectName;
		projectAlias: projectAlias;
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
					(project repositoryRoot / 'rowan' / 'specs' / projectAlias , 'ston') pathString)
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
	self assert: resolvedProject _loadComponents = project _loadComponents.
	project halt
]
