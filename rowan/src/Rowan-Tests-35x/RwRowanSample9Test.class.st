Class {
	#name : 'RwRowanSample9Test',
	#superclass : 'RwRowanSampleAbstractTest',
	#category : 'Rowan-Tests-35x'
}

{ #category : 'tests' }
RwRowanSample9Test >> testCreateNewProject [
	| projectName projectDefinition projectLoadSpecification |
	projectName := 'RowanSample9'.
	Rowan
		projectNamed: projectName
		ifPresent: [ :prj | Rowan image _removeLoadedProject: prj _loadedProject ].

	projectDefinition := (RwProjectDefinitionV2
		projectName: projectName
		comment: 'Example project exposing the standard project creation api')
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
		yourself.

	(STON toStringPretty: projectDefinition) halt.

	projectLoadSpecification := projectDefinition projectLoadSpecification.
]
