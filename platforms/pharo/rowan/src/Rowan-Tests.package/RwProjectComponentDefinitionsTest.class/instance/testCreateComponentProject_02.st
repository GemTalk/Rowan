tests
testCreateComponentProject_02


	"Create a Rowan project from scratch, using the available defaults"

	"execute without errors .. for now"

	| projectName project |
	projectName := 'Example'.
	Rowan projectNamed: projectName ifPresent: [:prj | prj unload ].

	project := RwComponentProjectDefinition
		projectName: projectName 
			projectHome: '$ROWAN_PROJECTS_HOME'
			useGit: true
			comment: 'Example project exposing the standard project creation api'.
	project
		addComponentNamed: 'Core'
			comment: 'Primary component used for loading entire system.';
		addPackagesNamed: { 'Example-Core1' . 'Example-Core2' };
		addPackageNamed: 'Example-Core3'
			withConditions: { 'common' };
		addPackageNamed: 'Example-Core4'
			withConditions: { 'common' };
		addPackageNamed: 'Rowan-Tests'
			withConditions: { 'common' }
			andGroupName: 'tests';
		addPackageNamed: 'Example-GemStone-Extensions'
			withConditions: { 'gemstone' }
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'Globals';
		addPackageNamed: 'Example-GemStone-Test-Extensions'
			withConditions: { 'gemstone' }
			andGroupName: 'tests'
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'Globals';
		yourself