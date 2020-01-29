Class {
	#name : 'RwSimpleComponentRowanExperiment',
	#superclass : 'RwAbstractV2Test',
	#instVars : [
		'originalComponents'
	],
	#category : 'Rowan-Strawman1'
}

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _createGemStoneComponents: resolvedProject [
	resolvedProject
		addSimpleNestedComponentNamed: 'org/platform/gemstone/Services'
			condition: 'gemstone'
			comment: 'Services gemstone packages';
		addComponentNamed: 'org/platform/gemstone/Services'
			toComponentNamed: 'org/Services';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed: 'org/platform/gemstoneBase/v2/Components'
			condition: 'gemstoneBase'
			comment: 'Components gemstoneBase packages';
		addComponentNamed: 'org/platform/gemstoneBase/v2/Components'
			toComponentNamed: 'org/Components';
		addSimpleNestedComponentNamed: 'org/platform/gemstone/v2/Components'
			condition: 'gemstone'
			comment: 'Components gemstone packages';
		addComponentNamed: 'org/platform/gemstone/v2/Components'
			toComponentNamed: 'org/Components';
		yourself
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _createGemStoneTestComponents: resolvedProject [
	resolvedProject
		addSimpleNestedComponentNamed: 'org/tests/platform/gemstone/Services'
			condition: 'gemstone'
			comment: 'Services gemstone test packages';
		addComponentNamed: 'org/tests/platform/gemstone/Services'
			toComponentNamed: 'org/tests/Services';
		yourself
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _createGemStoneTroubleComponents: resolvedProject [
	resolvedProject
		addPlatformNestedComponentNamed: 'org/platform/gemstone/Core'
			condition:
				{'gemstone'.
					'gemstoneBase'}
			comment: 'Core gemstone packages';
		addComponentNamed: 'platform/gemstone/Core' toComponentNamed: 'org/platform/Core';
		yourself.



	resolvedProject
		addPlatformNestedComponentNamed: 'org/platform/gemstone/Components'
			condition:
				{'gemstone'.
					'gemstoneBase'}
			comment: 'Components gemstone packages';
		addComponentNamed: 'platform/gemstone/Components'
			toComponentNamed: 'org/Components';
		addPlatformNestedComponentNamed: 'org/platform/gemstone/Specifications'
			condition:
				{'gemstone'.
					'gemstoneBase'}
			comment: 'Specifications gemstone packages';
		addComponentNamed: 'org/platform/gemstone/Specifications'
			toComponentNamed: 'org/Specifications';
		yourself
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _createPlatformComponents: resolvedProject [
	resolvedProject
		addSimpleNestedComponentNamed: 'org/platform/Components'
			condition: 'common'
			comment: 'Components platform packages';
		addComponentNamed: 'org/platform/Components'
			toComponentNamed: 'org/Components';
		addSimpleNestedComponentNamed: 'org/platform/Core'
			condition: 'common'
			comment: 'Core platform packages';
		addComponentNamed: 'org/platform/Core' toComponentNamed: 'org/Core';
		addSimpleNestedComponentNamed: 'org/platform/Definitions'
			condition: 'common'
			comment: 'Definitions platform packages';
		addComponentNamed: 'org/platform/Definitions'
			toComponentNamed: 'org/Definitions';
		addSimpleNestedComponentNamed: 'org/platform/Kernel'
			condition: 'common'
			comment: 'Kernel platform packages';
		addComponentNamed: 'org/platform/Kernel' toComponentNamed: 'org/Kernel';
		addSimpleNestedComponentNamed: 'org/platform/Loader'
			condition: 'common'
			comment: 'Loader platform packages';
		addComponentNamed: 'org/platform/Loader' toComponentNamed: 'org/Loader';
		addSimpleNestedComponentNamed: 'org/platform/Services'
			condition: 'common'
			comment: 'Services platform packages';
		addComponentNamed: 'org/platform/Services' toComponentNamed: 'org/Services';
		addSimpleNestedComponentNamed: 'org/platform/Specifications'
			condition: 'common'
			comment: 'Specifications platform packages';
		addComponentNamed: 'org/platform/Specifications'
			toComponentNamed: 'org/Specifications';
		addComponentNamed: 'org/platform/Services' toComponentNamed: 'org/Services';
		addSimpleNestedComponentNamed: 'org/platform/Tools'
			condition: 'common'
			comment: 'Tools platform packages';
		addComponentNamed: 'org/platform/Tools' toComponentNamed: 'org/Tools';
		yourself
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _createRowanComponents: resolvedProject [
	resolvedProject
		addSimpleNestedComponentNamed: 'org/Specifications'
			condition: 'common'
			comment:
				'Organize all specification classes under one component. Shared between Rowan and RowanDiskAPI.';
		addComponentNamed: 'org/Specifications' toComponentNamed: 'Rowan';
		addComponentNamed: 'org/Specifications' toComponentNamed: 'RowanDiskAPI';
		addSimpleNestedComponentNamed: 'org/Components'
			condition: 'common'
			comment:
				'Organize all component classes under one component. Shared between Rowan and RowanDiskAPI.';
		addComponentNamed: 'org/Components' toComponentNamed: 'Rowan';
		addComponentNamed: 'org/Components' toComponentNamed: 'RowanDiskAPI';
		addSimpleNestedComponentNamed: 'org/Tools'
			condition: 'common'
			comment:
				'Organize all tool classes under one component. Shared between Rowan and RowanDiskAPI.';
		addComponentNamed: 'org/Tools' toComponentNamed: 'Rowan';
		addComponentNamed: 'org/Tools' toComponentNamed: 'RowanDiskAPI';
		addSimpleNestedComponentNamed: 'org/Core'
			condition: 'common'
			comment:
				'Organize all core classes under one component. Shared between Rowan and RowanDiskAPI.';
		addComponentNamed: 'org/Core' toComponentNamed: 'Rowan';
		addComponentNamed: 'org/Core' toComponentNamed: 'RowanDiskAPI';
		yourself.	"Components that are shared between Rowan and RowanDiskAPI"

	resolvedProject
		addSimpleNestedComponentNamed: 'org/Kernel'
			condition: 'common'
			comment:
				'Organize all kernel classes under one component. Kernel classes are those core 
	classes that are not present as part of the RowanDiskAPI, most notably, the class Rowan.';
		addComponentNamed: 'org/Kernel' toComponentNamed: 'Rowan';
		addSimpleNestedComponentNamed: 'org/Definitions'
			condition: 'common'
			comment: 'Organize all definition classes under one component';
		addComponentNamed: 'org/Definitions' toComponentNamed: 'Rowan';
		addSimpleNestedComponentNamed: 'org/Services'
			condition: 'common'
			comment: 'Organize all services classes under one component';
		addComponentNamed: 'org/Services' toComponentNamed: 'Rowan';
		addSimpleNestedComponentNamed: 'org/Loader'
			condition: 'common'
			comment: 'Organize all loader classes under one component';
		addComponentNamed: 'org/Loader' toComponentNamed: 'Rowan';
		yourself	"Components that are used only in Rowan"
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _createRowanTestComponents: resolvedProject [
	resolvedProject
		addSimpleNestedComponentNamed: 'org/tests/Core'
			condition: 'tests'
			comment: 'Organize all core tests under one component';
		addComponentNamed: 'org/tests/Core' toComponentNamed: 'org/Core';
		addSimpleNestedComponentNamed: 'org/tests/Specifications'
			condition: 'tests'
			comment: 'Organize all specification tests under one component';
		addComponentNamed: 'org/tests/Specifications'
			toComponentNamed: 'org/Specifications';
		addSimpleNestedComponentNamed: 'org/tests/Components'
			condition: 'tests'
			comment: 'Organize all component tests under one component';
		addComponentNamed: 'org/tests/Components' toComponentNamed: 'org/Components';
		addSimpleNestedComponentNamed: 'org/tests/Services'
			condition: 'tests'
			comment: 'Organize all service tests under one component';
		addComponentNamed: 'org/tests/Services' toComponentNamed: 'org/Services';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed: 'org/tests/v2/Specifications'
			condition: 'v2'
			comment: 'Organize all specification tests under one component';
		addComponentNamed: 'org/tests/v2/Specifications'
			toComponentNamed: 'org/tests/Specifications';
		addSimpleNestedComponentNamed: 'org/tests/v2/Components'
			condition: 'v2'
			comment: 'Organize all component tests under one component';
		addComponentNamed: 'org/tests/v2/Components'
			toComponentNamed: 'org/tests/Components';
		yourself
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _createTopLevelComponents: resolvedProject [
	resolvedProject
		addSimpleComponentNamed: 'Rowan'
			condition: 'common'
			comment: 'top-level component for loading entire Rowan project.';
		addSimpleComponentNamed: 'RowanDiskAPI'
			condition: 'common'
			comment:
				'top-level component for loading Rowan API for reading writing Rowan disk structure.';
		yourself.

	(resolvedProject componentNamed: 'Rowan')
		addProjectNamed: 'FileSystemGs';
		addProjectNamed: 'STON';
		yourself.

	(resolvedProject componentNamed: 'RowanDiskAPI')
		addProjectNamed: 'FileSystemGs';
		addProjectNamed: 'STON';
		yourself.
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _createV2Components: resolvedProject [
	resolvedProject
		addSimpleNestedComponentNamed: 'org/v2/Specifications'
			condition: 'v2'
			comment: 'Organize all v2 specification classes under one component';
		addComponentNamed: 'org/v2/Specifications'
			toComponentNamed: 'org/Specifications';
		addSimpleNestedComponentNamed: 'org/v2/Components'
			condition: 'v2'
			comment: 'Organize all v2 component classes under one component';
		addComponentNamed: 'org/v2/Components'
			toComponentNamed: 'org/Components';
		yourself
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _populateCommonComponentPackages: resolvedProject [
	"
		subs/DiskConfigurationsCommon common/core
		subs/DiskConfigurationsCommonV2 common/tests
	"

	(resolvedProject componentNamed: 'org/Core')
		addPackageNames:
				{'Rowan-Url-Core'.
					'Rowan-Core-Conditional-Support'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Core-Conditional-Support'
			setSymbolDictNameTo: 'RowanTools';
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Specifications'
			setSymbolDictNameTo: 'RowanTools';
		yourself.

	(resolvedProject componentNamed: 'org/Definitions')
		addPackageNames: {'Rowan-Definitions-Common'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Definitions-Common'
			setSymbolDictNameTo: 'RowanTools';
		yourself.

	(resolvedProject componentNamed: 'org/Specifications')
		addPackageNames: {'Rowan-Specifications'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Specifications'
			setSymbolDictNameTo: 'RowanTools';
		yourself.

	(resolvedProject componentNamed: 'org/tests/Core')
		addPackageNames: {'Rowan-Tests-Core'};
		yourself.
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _populateGemStoneComponentPackages: resolvedProject [
	"
		subs/DiskConfigurationsCommon gemstone/core
		subs/Services gemstone/core
		subs/DiskConfigurationsCommonV2 gemstone/core
	"

	(resolvedProject componentNamed: 'org/platform/gemstone/Core')
		addPackageNames: {'Rowan-GemStone-Url'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-Url'
			setSymbolDictNameTo: 'Globals';
		yourself.

	(resolvedProject componentNamed: 'org/platform/gemstone/Components')
		addPackageNames:
				{'Rowan-GemStone-Components-Kernel'.
					'Rowan-Components-Kernel'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-Components-Kernel'
			setSymbolDictNameTo: 'Globals';
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Components-Kernel'
			setSymbolDictNameTo: 'Globals';
		yourself.

	(resolvedProject componentNamed: 'org/platform/gemstone/Specifications')
		addPackageNames: {'Rowan-GemStone-Specifications'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-Specifications'
			setSymbolDictNameTo: 'RowanTools';
		yourself.

	(resolvedProject componentNamed: 'org/platform/gemstone/Services')
		addPackageNames:
				{'Rowan-Services-Core'.
					'Rowan-Services-Extensions'.
					'Rowan-JadeServer'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-JadeServer'
			setSymbolDictNameTo: 'UserGlobals';
		yourself.

	(resolvedProject componentNamed: 'org/platform/gemstoneBase/v2/Components')
		addPackageNames:
				{'Rowan-GsBase-ComponentsV2'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GsBase-ComponentsV2'
			setSymbolDictNameTo: 'RowanTools';
		yourself.

	(resolvedProject componentNamed: 'org/platform/gemstone/v2/Components')
		addPackageNames:
				{'Rowan-GemStone-ComponentsV2'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-ComponentsV2'
			setSymbolDictNameTo: 'RowanTools';
		yourself.
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _populateV2ComponentPackages: resolvedProject [
	"
		subs/DiskConfigurationsCommonV2 common/core
	"

	(resolvedProject componentNamed: 'org/v2/Components')
		addPackageNames: {'Rowan-ComponentsV2'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-ComponentsV2'
			setSymbolDictNameTo: 'RowanTools';
		yourself.

	(resolvedProject componentNamed: 'org/v2/Specifications')
		addPackageNames: {'Rowan-SpecificationsV2'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-SpecificationsV2'
			setSymbolDictNameTo: 'RowanTools';
		yourself.

	(resolvedProject componentNamed: 'org/tests/v2/Components')
		addPackageNames: {'Rowan-Tests-ComponentsV2'};
		yourself.
	(resolvedProject componentNamed: 'org/tests/v2/Specifications')
		addPackageNames: {'Rowan-Tests-SpecificationsV2'};
		yourself
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _readRowanFromDisk: specArray [
	| loadSpecUrl loadSpec projectsHome conditionalAttributes |
	loadSpecUrl := specArray at: 1.
	projectsHome := specArray at: 2.
	conditionalAttributes := specArray at: 3.
	loadSpec := RwSpecification fromUrl: loadSpecUrl.
	^ loadSpec
		projectsHome: projectsHome;
		resolve
]

{ #category : 'tests' }
RwSimpleComponentRowanExperiment >> testBuildExperimentalRowanComponentStructure [
	"Read project and packages from disk."

	"
		subs/DiskConfigurationsCommon common/core
		subs/DiskConfigurationsCommon gemstone/core

		subs/Services gemstone/core
	"

	| specArray resolvedProject loadSpec |
	false
		ifTrue: [ ^ self ].	"remove this statement if you want to generate the structures"
	specArray := {'file:$ROWAN_PROJECTS_HOME/Rowan/rowan/v2/specs/ComponentV2.ston'.
	'$ROWAN_PROJECTS_HOME'.
	{'common'.
	'gemstone'.
	'gemstone-kernel'.
	('3.5.0' asRwGemStoneVersionNumber)	"want to read 3.5.0 packages"}}.
	resolvedProject := self _readRowanFromDisk: specArray.

	resolvedProject _projectSpecification
		specName: 'proposed_project';
		componentsPath: 'rowan/v2/proposed_components';
		specsPath: 'rowan/v2/proposed_specs'.	"write components in a new spot"
	resolvedProject componentsRoot ensureDeleteAll.
	originalComponents := resolvedProject _projectDefinition components.	"save for reference"
	resolvedProject _projectDefinition components: RwResolvedLoadComponentsV2 new.	"clear existing components"
	self
		_createTopLevelComponents: resolvedProject;
		_createRowanComponents: resolvedProject;
		_createRowanTestComponents: resolvedProject;
		_createV2Components: resolvedProject;
		_createPlatformComponents: resolvedProject;
		_createGemStoneTroubleComponents: resolvedProject;
		_createGemStoneComponents: resolvedProject;
		_createGemStoneTestComponents: resolvedProject;
		_populateCommonComponentPackages: resolvedProject;
		_populateGemStoneComponentPackages: resolvedProject;
		_populateV2ComponentPackages: resolvedProject;
		yourself.
	resolvedProject
		exportComponents;
		exportProjectSpecification;
		yourself.

	loadSpec := resolvedProject _loadSpecification copy.
	loadSpec
		specName: 'Rowan_proposed';
		projectSpecFile: 'rowan/v2/proposed_project.ston';
		revision: 'candidateV2.0';
		componentNames: {'Rowan'};
		groupNames: nil;
		comment: 'Load spec for loading Rowan v2';
		customConditionalAttributes:
				{'tests'.
					'v2'};
		exportTo: resolvedProject specsRoot;
		yourself.

	loadSpec := loadSpec copy.
	loadSpec
		specName: 'RowanDiskAPI_proposed';
		componentNames: {'RowanDiskAPI'};
		comment: 'Load spec for loading Rowan disk API for v2';
		customConditionalAttributes:
				{'tests'.
					'v2'};
		exportTo: resolvedProject specsRoot;
		yourself.

	loadSpec := loadSpec copy.
	loadSpec
		specName: 'RowanV1_proposed';
		componentNames: {'Rowan'};
		comment: 'Load spec for loading Rowan v1';
		customConditionalAttributes:
				{'tests'.
					'v1'};
		exportTo: resolvedProject specsRoot;
		yourself.

	loadSpec := loadSpec copy.
	loadSpec
		specName: 'ComponentV2_proposed';
		componentNames: {'Rowan'};
		comment:
				'Load spec for loading Rowan Components v2 - current development image (v1, v2, components) components will be deprecated';
		customConditionalAttributes:
				{'tests'.
					'v1'.
					'v2'.
					'components'};
		exportTo: resolvedProject specsRoot;
		yourself
]
