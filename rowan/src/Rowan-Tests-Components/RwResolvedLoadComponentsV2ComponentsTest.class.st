Class {
	#name : 'RwResolvedLoadComponentsV2ComponentsTest',
	#superclass : 'RwAbstractLoadComponentV2TestComponentsTest',
	#category : 'Rowan-Tests-Components'
}

{ #category : 'tests' }
RwResolvedLoadComponentsV2ComponentsTest >> testBasic [
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
			comment:
				'Nested component defines a shared subsystem of the primary project.';
		addComponentNamed: 'Tests'
			definedGroupNames:
				(Dictionary new
						add: 'core' -> {};
						add: 'tests' -> {'core'};
						yourself)
			comment: 'Contains all of the project tests.';
		yourself.

	resolvedComponents
		addProjectNamed: 'DependentProject'
			toComponentNamed: 'Core'
			withConditions: {'common'}
			andGroupName: 'core';
		addComponentNamed: 'Tests'
			toComponentNamed: 'Core'
			withConditions: {'common'}
			andGroupName: 'tests';
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
			toComponentNamed: 'Tests'
			withConditions: {'gemstone'}
			andGroupName: 'tests';
		addPackagesNamed: {(projectName , '-Shared')}
			toComponentNamed: 'subs/Nested'
			withConditions: {'common'}
			andGroupName: 'core';
		gemstoneSetSymbolDictName: 'Globals' forPackageNamed: projectName , '-Shared';
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
		assert:
			(resolvedComponents
				gemstoneSymbolDictNameForPackageNamed: projectName , '-Shared'
				forUser: 'AUser'
				ifAbsent: [  ]) = 'Globals'.


	self
		_validateComponent: (resolvedComponents componentNamed: 'Core')
			condition: {'common'}
			group: 'core'
			componentNames: {'subs/Nested'}
			packageNames: {'ResolvedLoadComponentsV2Test-Core'}
			projectNames: {'DependentProject'};
		_validateComponent: (resolvedComponents componentNamed: 'Core')
			condition: {'common'}
			group: 'tests'
			componentNames: {'Tests'}
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
			packageNames: {}
			projectNames: {};
		yourself.
	self
		_validateComponent: (resolvedComponents componentNamed: 'Tests')
			condition: {'common'}
			group: 'tests'
			componentNames: {}
			packageNames: {}
			projectNames: {};
		_validateComponent: (resolvedComponents componentNamed: 'Tests')
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
		yourself.

	resolvedComponents removePackageNamed: 'ResolvedLoadComponentsV2Test-Shared'.
	stonString := STON toStringPretty: resolvedComponents.	"useful to view full structure"

	allPackageNames := resolvedComponents _validate.
	self
		assert:
			allPackageNames asArray sort
				=
					#('ResolvedLoadComponentsV2Test-Tests' 'ResolvedLoadComponentsV2Test-GemStone-Tests' 'ResolvedLoadComponentsV2Test-Core' 'ResolvedLoadComponentsV2Test-GemStone-Extensions')
						sort.

	self
		_validateComponent: (resolvedComponents componentNamed: 'subs/Nested')
			condition: {'common'}
			group: 'core'
			componentNames: {}
			packageNames: {}
			projectNames: {};
		yourself.

	resolvedComponents removeComponentNamed: 'subs/Nested'.
	stonString := STON toStringPretty: resolvedComponents.	"useful to view full structure"

	allPackageNames := resolvedComponents _validate.
	self
		assert:
			allPackageNames asArray sort
				=
					#('ResolvedLoadComponentsV2Test-Tests' 'ResolvedLoadComponentsV2Test-GemStone-Tests' 'ResolvedLoadComponentsV2Test-Core' 'ResolvedLoadComponentsV2Test-GemStone-Extensions')
						sort.

	self
		_validateComponent: (resolvedComponents componentNamed: 'Core')
			condition: {'common'}
			group: 'core'
			componentNames: {}
			packageNames: {'ResolvedLoadComponentsV2Test-Core'}
			projectNames: {'DependentProject'};
		yourself.
	self
		assert:
			(resolvedComponents componentNamed: 'subs/Nested' ifAbsent: [  ]) isNil.

	resolvedComponents removeProjectNamed: 'DependentProject'.
	stonString := STON toStringPretty: resolvedComponents.	"useful to view full structure"

	allPackageNames := resolvedComponents _validate.
	self
		assert:
			allPackageNames asArray sort
				=
					#('ResolvedLoadComponentsV2Test-Tests' 'ResolvedLoadComponentsV2Test-GemStone-Tests' 'ResolvedLoadComponentsV2Test-Core' 'ResolvedLoadComponentsV2Test-GemStone-Extensions')
						sort.

	self
		_validateComponent: (resolvedComponents componentNamed: 'Core')
			condition: {'common'}
			group: 'core'
			componentNames: {}
			packageNames: {'ResolvedLoadComponentsV2Test-Core'}
			projectNames: {};
		yourself.
]
