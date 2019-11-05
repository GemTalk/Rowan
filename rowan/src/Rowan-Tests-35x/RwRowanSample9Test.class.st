Class {
	#name : 'RwRowanSample9Test',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-Tests-35x'
}

{ #category : 'tests' }
RwRowanSample9Test >> testCreateNewProject [
	| projectName projectDefinition projectLoadSpecification packageName |
	projectName := 'RowanSample9'.
	projectDefinition := (RwProjectDefinitionV2
		projectName: projectName
		comment: 'Project created using the V2 project creation api ')
		addComponentNamed: 'Core'
			definedGroupNames:
				(Dictionary new
					add: 'core' -> {};
					add: 'tests' -> {'core'};
					yourself)
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'ExampleCore'
			comment: 'Primary component used for loading the system.';
		addPackagesNamed:
				{(projectName , '-Core1').
				(projectName , '-Core2')}
			toComponentNamed: 'Core'
			withConditions: {'common'}
			andGroupName: 'core';
		addPackageNamed: projectName , '-Tests'
			toComponentNamed: 'Core'
			withConditions: {'common'}
			andGroupName: 'tests';
		addPackageNamed: projectName , '-GemStone-Extensions'
			toComponentNamed: 'Core'
			withConditions: {'gemstone'}
			andGroupName: 'core'
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'Globals';
		addPackageNamed: projectName , '-GemStone-Test-Extensions'
			toComponentNamed: 'Core'
			withConditions: {'gemstone'}
			andGroupName: 'tests'
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'Globals';
		yourself.
	packageName := projectName , '-Core1'.
	(projectDefinition packageNamed: packageName)
		addClassNamed: projectName , 'Class1'
			super: 'Object'
			instvars: #('ivar1')
			category: '*' , packageName asLowercase
			comment: 'I am an example class';
		yourself.
	projectLoadSpecification := projectDefinition projectLoadSpecification.
	{(STON toStringPretty: projectDefinition).
	(STON toStringPretty: projectLoadSpecification)}
]
