Class {
	#name : 'RwEnvyProjectLoadComponentTest',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-Strawman1'
}

{ #category : 'tests' }
RwEnvyProjectLoadComponentTest >> testBasic [
	"Create project using envy application and subapplication components. 
		Based on project created in RwStrawmanProjectTestsV2>>testBasic"

	"Abandoned/deferred work on RwEnvyApplicationProjectLoadComponentV2 (https://github.com/GemTalk/Rowan/issues/553#issuecomment-579028291), because envy application/subaapplications not quite suitable for mainstream Rowan use ... leaving (failing) test in case we need application/subapplication for VAST support. If not will repurpose using simple components."

	| projectName project stonString loadSpec resolvedProject x y |
	projectName := 'EnvyApplication'.
	project := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		comment: 'Project created for testing the ENVY application and subapplication components';
		yourself.

	project repositoryRoot ensureDeleteAll.

"Create the set of applications and subapplications"
	project
		addApplicationNamed: 'CommonApplication'
			condition: 'common'
			comment: 
	'This application is commn to vast and gemstone and will have a common subapplication
	which will itself have subapplications that are conditional on vast and gemstone';
		addApplicationNamed: 'VASTApplication'
			condition: 'vast'
			comment: 
	'This application is conditional on VAST ... it will have SUnit as a prereq, 
	which is resolved within a VAST image';

		addSubApplicationNamed: 'subs/SubApplicationCommon'
			condition: 'common'
			comment: 
	'Common subapplication';
		addSubApplicationNamed: 'subs/SubApplicationVast'
			condition: 'vast'
			comment: 
	'VAST subapplication';
		addSubApplicationNamed: 'subs/SubApplicationGemStone'
			condition: 'gemstone'
			comment: 
	'GemStone application';

		addApplicationNamed: 'VASTApplication'
			toComponentNamed: 'CommonApplication';
		addSubApplicationNamed: 'subs/SubApplicationCommon'
			toComponentNamed: 'CommonApplication';

"SUnit is intended to be resolved within the VAST image itself"
		addApplicationNamed: 'SUnit'
			toComponentNamed: 'VASTApplication';

		addSubApplicationNamed: 'subs/SubApplicationGemStone'
			toComponentNamed: 'subs/SubApplicationCommon';
		addSubApplicationNamed: 'subs/SubApplicationVast'
			toComponentNamed: 'subs/SubApplicationCommon';
			
"now we create the packages and distribute them amongst the applications"

		addPackagesNamed:
				{'TonelExampleApp' . 'TonelExampleSubApp'}
			toComponentNamed: 'subs/SubApplicationCommon';
		addPackagesNamed:
				{'TonelExampleForGemStoneApp'}
			toComponentNamed: 'subs/SubApplicationGemStone';
		addPackagesNamed:
				{'TonelExampleAnotherSubSubApp'}
			toComponentNamed: 'subs/SubApplicationVast';
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
