Class {
	#name : 'RwResolvedLoadComponentsV2Test',
	#superclass : 'RwAbstractLoadComponentV2Test',
	#category : 'Rowan-Tests-DiskConfigurationsV2'
}

{ #category : 'tests' }
RwResolvedLoadComponentsV2Test >> testBasic [
	"excercise basic functionality for RwResolvedLoadComponentV2"

	| projectName resolvedComponents allPackageNames stonString |
	projectName := 'ResolvedLoadComponentsV2Test'.
	resolvedComponents := RwResolvedLoadComponentsV2 new.
	(resolvedComponents
		addComponentNamed: 'Core'
		definedGroupNames:
			(Dictionary new
				add: 'core' -> {};
				add: 'tests' -> {'core'};
				yourself)
		comment: 'Primary component used for loading the system.')
		conditionalPropertiesAtConditions: {'common'}
			andGroup: 'core'
			addComponentNames: {'subs/Nested'};
		yourself.
	resolvedComponents
		addNestedComponentNamed: 'subs/Nested'
		definedGroupNames:
			(Dictionary new
				add: 'core' -> {};
				add: 'tests' -> {'core'};
				yourself)
		comment: 'Nested component defines a shared subsystem of the primary project.'.

	resolvedComponents
		addPackagesNamed: {(projectName , '-Core')}
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
		addPackageNamed: projectName , '-GemStone-Tests'
			toComponentNamed: 'Core'
			withConditions: {'gemstone'}
			andGroupName: 'tests';
		addPackagesNamed: {(projectName , '-Shared')}
			toComponentNamed: 'subs/Nested'
			withConditions: {'common'}
			andGroupName: 'core';
		yourself.

	stonString := STON toStringPretty: resolvedComponents.	"useful to view full structure"
	allPackageNames := resolvedComponents _validate.
	self
		assert:
			allPackageNames asArray sort
				=
					#('ResolvedLoadComponentsV2Test-Tests' 'ResolvedLoadComponentsV2Test-GemStone-Tests' 'ResolvedLoadComponentsV2Test-Shared' 'ResolvedLoadComponentsV2Test-Core' 'ResolvedLoadComponentsV2Test-GemStone-Extensions')
						sort.

	self
		_validateComponent: (resolvedComponents componentNamed: 'Core')
			condition: {'common'}
			group: 'core'
			componentNames: {'subs/Nested'}
			packageNames: {'ResolvedLoadComponentsV2Test-Core'}
			projectNames: {};
		_validateComponent: (resolvedComponents componentNamed: 'Core')
			condition: {'common'}
			group: 'tests'
			componentNames: {}
			packageNames: {'ResolvedLoadComponentsV2Test-Tests'}
			projectNames: {};
		_validateComponent: (resolvedComponents componentNamed: 'Core')
			condition: {'gemstone'}
			group: 'core'
			componentNames: {}
			packageNames: {'ResolvedLoadComponentsV2Test-GemStone-Extensions'}
			projectNames: {};
		_validateComponent: (resolvedComponents componentNamed: 'Core')
			condition: {'gemstone'}
			group: 'tests'
			componentNames: {}
			packageNames: {'ResolvedLoadComponentsV2Test-GemStone-Tests'}
			projectNames: {};
		yourself.
	self
		_validateComponent: (resolvedComponents componentNamed: 'subs/Nested')
			condition: {'common'}
			group: 'core'
			componentNames: {}
			packageNames: {'ResolvedLoadComponentsV2Test-Shared'}
			projectNames: {};
		yourself
]
