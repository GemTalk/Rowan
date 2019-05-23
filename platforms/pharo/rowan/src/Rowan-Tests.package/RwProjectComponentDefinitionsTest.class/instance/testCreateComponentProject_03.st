tests
testCreateComponentProject_03

	"Create a Rowan project from scratch, using array-based apis for sets of packages that have the same
		package attributes"

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
		addPackagesNamed: {'Example-Core1' . 'Example-Core2'}
			withConditions: { 'common' };
		addPackagesNamed: { 'Example-Tests1' . 'Example-Tests2' }
			withConditions: { 'common' }
			andGroupName: 'tests';
		addPackagesNamed: { 'Example-Test-Extensions1' . 'Example-Test-Extensions2' }
			toComponentNamed: 'Core'
			withConditions: { 'common' }
			andGroupName: 'tests';
		addPackagesNamed: { 'Example-GemStone-Extensions1' . 'Example-GemStone-Extensions2' }
			toComponentNamed: 'Core'
			withConditions: { 'gemstone' }
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'Globals';
		addPackagesNamed: { 'Example-GemStone-Extensions3' . 'Example-GemStone-Extensions4' }
			withConditions: { 'gemstone' }
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'Globals';
		addPackagesNamed: { 'Example-GemStone-Test-Extensions1' . 'Example-GemStone-Test-Extensions2' }
			toComponentNamed: 'Core'
			withConditions: { 'gemstone' }
			andGroupName: 'tests'
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'Globals';
		addPackagesNamed: { 'Example-GemStone-Test-Extensions3' . 'Example-GemStone-Test-Extensions4' }
			withConditions: { 'gemstone' }
			andGroupName: 'tests'
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'Globals';
		yourself