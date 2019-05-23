tests
testCreateComponentProject_01

	"create Rowan project from scratch"

	"execute without errors .. for now"

	| projectName project |
	projectName := 'Example'.
	Rowan projectNamed: projectName ifPresent: [:prj | prj unload ].

	project := RwComponentProjectDefinition
		projectName: projectName 
			componentNamesToLoad: #( 'Core' )
			groupNamesToLoad: #( 'core' 'tests' )
			defaultComponentName: 'Core'
			defaultGroupName: 'core'
			packageFormat: 'tonel'
			projectHome: '$ROWAN_PROJECTS_HOME'
			specsPath: 'rowan/specs'
			componentsPath: 'rowan/componenets'
			packagesPath: 'rowan/src'
			projectsPath: 'rowan/projects'
			useGit: true
			comment: 'Example project exposing the standard project creation api'.
	project
		addComponentNamed: 'Core'
			definedGroupNames: 
				(Dictionary new
					add: 'core'  -> {};
					add: 'tests' -> { 'core' };
					yourself)
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'ExampleCore'
			comment: 'Primary component used for loading entire system.';
		addPackageNamed: 'Example-Core1'
			withConditions: { 'common' };
		addPackageNamed: 'Example-Core2'
			withConditions: { 'common' };
		addPackageNamed: 'Example-Tests'
			withConditions: { 'common' }
			andGroupName: 'tests';
		addPackageNamed: 'Example-Test-Extensions'
			toComponentNamed: 'Core'
			withConditions: { 'gemstone' }
			andGroupName: 'tests';
		addPackageNamed: 'Example-GemStone-Extensions'
			toComponentNamed: 'Core'
			withConditions: { 'gemstone' }
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'Globals';
		addPackageNamed: 'Example-GemStone-Test-Extensions'
			toComponentNamed: 'Core'
			withConditions: { 'gemstone' }
			andGroupName: 'tests'
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'Globals';
		yourself