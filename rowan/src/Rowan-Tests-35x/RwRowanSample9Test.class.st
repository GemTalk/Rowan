Class {
	#name : 'RwRowanSample9Test',
	#superclass : 'RwRowanSampleAbstractTest',
	#category : 'Rowan-Tests-35x'
}

{ #category : 'tests' }
RwRowanSample9Test >> testCreateNewProject [
"should get errors if groups or components are referenced that were not declared"

"the default spec should reflect the entire project as created, so perhaps default values shouldn't be used for components and groups when creating a new project ... forNewProject, which skips initialization of those things that need to be explicitly created in the project"

	| projectName projectDefinition projectLoadSpecification |
	projectName := 'RowanSample9'.
	Rowan
		projectNamed: projectName
		ifPresent: [ :prj | Rowan image _removeLoadedProject: prj _loadedProject ].

	projectDefinition := (RwProjectDefinitionV2
		projectName: projectName
		comment: 'Example project exposing the standard project creation api')
		addComponentNamed: 'Core'
			definedGroupNames: 
				(Dictionary new
					add: 'core'  -> {};
					add: 'tests' -> { 'core' };
					yourself)
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'ExampleCore'
			comment: 'Primary component used for loading entire system.';
		addPackagesNamed: { 'Example-Core1' . 'Example-Core2' }
			toComponentNamed: 'Core'
			withConditions: { 'common' }
			andGroupName: 'tests';
		addPackageNamed: 'Rowan-Tests'
			toComponentNamed: 'Core'
			withConditions: { 'common' }
			andGroupName: 'tests';
		addPackageNamed: 'Example-GemStone-Extensions'
			toComponentNamed: 'Core'
			withConditions: { 'gemstone' }
			andGroupName: 'core'
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'Globals';
		addPackageNamed: 'Example-GemStone-Test-Extensions'
			toComponentNamed: 'Core'
			withConditions: { 'gemstone' }
			andGroupName: 'tests'
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'Globals';
		yourself.

	projectLoadSpecification := projectDefinition projectLoadSpecification.

	{ 
		(STON toStringPretty: projectDefinition).
		(STON toStringPretty: projectLoadSpecification).
	}halt.
]
