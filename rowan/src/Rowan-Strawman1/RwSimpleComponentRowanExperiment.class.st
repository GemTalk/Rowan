Class {
	#name : 'RwSimpleComponentRowanExperiment',
	#superclass : 'RwAbstractV2Test',
	#instVars : [
		'originalComponents'
	],
	#category : 'Rowan-Strawman1'
}

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _createComponentsV2Components: resolvedProject [
	resolvedProject
		addSimpleNestedComponentNamed: 'common/componentsV2/Components'
			condition: 'componentsV2'
			comment:
				'Organize all componentsV2 components classes under one component. componentsV2 will eventually be deprecated, when v2 is fully functional.';
		addComponentNamed: 'common/componentsV2/Components'
			toComponentNamed: 'common/Components';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed: 'common/tests/componentsV2/Components'
			condition: 'componentsV2'
			comment: 'Organize all Components tests under one component';
		addComponentNamed: 'common/tests/componentsV2/Components'
			toComponentNamed: 'common/tests/Components';
		yourself.

	(resolvedProject componentNamed: 'common/componentsV2/Components')
		addProjectNamed: 'Cypress';
		addProjectNamed: 'Tonel';
		yourself
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _createGemStoneComponents: resolvedProject [
	resolvedProject
		addSimpleNestedComponentNamed: 'common/v2/platform/gemstone/Core'
			condition: 'gemstone'
			comment: 'Core vw gemstone packages';
		addComponentNamed: 'common/v2/platform/gemstone/Core'
			toComponentNamed: 'common/v2/Core';
		addSimpleNestedComponentNamed: 'common/v2/platform/gemstone/Definitions'
			condition: 'gemstone'
			comment: 'Definitions vw gemstone packages';
		addComponentNamed: 'common/v2/platform/gemstone/Definitions'
			toComponentNamed: 'common/v2/Definitions';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed:
				'common/tests/v1/deprecated/platform/gemstone/Tests'
			condition: 'gemstone'
			comment:
				'Organize all test v1 deprecated gemstone classes under one component';
		addComponentNamed: 'common/tests/v1/deprecated/platform/gemstone/Tests'
			toComponentNamed: 'common/tests/v1/deprecated/Tests';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed: 'common/v1/platform/gemstone/Core'
			condition: 'gemstone'
			comment: 'Core gemstone packages';
		addComponentNamed: 'common/v1/platform/gemstone/Core'
			toComponentNamed: 'common/v1/Core';
		addSimpleNestedComponentNamed: 'common/v1/platform/gemstone/Specifications'
			condition: 'gemstone'
			comment: 'Specifications gemstone packages';
		addComponentNamed: 'common/v1/platform/gemstone/Specifications'
			toComponentNamed: 'common/v1/Specifications';
		addSimpleNestedComponentNamed: 'common/v1/platform/gemstone/Definitions'
			condition: 'gemstone'
			comment: 'Definitions gemstone packages';
		addComponentNamed: 'common/v1/platform/gemstone/Definitions'
			toComponentNamed: 'common/v1/Definitions';
		addSimpleNestedComponentNamed: 'common/v1/deprecated/platform/gemstone/Loader'
			condition: 'gemstone'
			comment: 'Services gemstone packages';
		addComponentNamed: 'common/v1/deprecated/platform/gemstone/Loader'
			toComponentNamed: 'common/v1/deprecated/Loader';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed: 'common/platform/gemstone/Services'
			condition: 'gemstone'
			comment: 'Services gemstone packages';
		addComponentNamed: 'common/platform/gemstone/Services'
			toComponentNamed: 'common/Services';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed: 'common/v2/platform/gemstoneBase/Components'
			condition: 'gemstoneBase'
			comment: 'Components gemstoneBase packages';
		addComponentNamed: 'common/v2/platform/gemstoneBase/Components'
			toComponentNamed: 'common/v2/Components';
		addSimpleNestedComponentNamed: 'common/v2/platform/gemstone/Components'
			condition: 'gemstone'
			comment: 'Components gemstone packages';
		addComponentNamed: 'common/v2/platform/gemstone/Components'
			toComponentNamed: 'common/v2/Components';
		yourself.

	resolvedProject
		addPlatformNestedComponentNamed: 'common/platform/gemstone_gemstoneBase/Core'
			condition:
				{'gemstone'.
					'gemstoneBase'}
			comment: 'Core gemstone or gemstoneBase packages';
		addComponentNamed: 'common/platform/gemstone_gemstoneBase/Core'
			toComponentNamed: 'common/Core';
		yourself.

	resolvedProject
		addPlatformNestedComponentNamed:
				'common/platform/gemstone_gemstoneBase/Components'
			condition:
				{'gemstone'.
					'gemstoneBase'}
			comment: 'Components gemstone or gemstoneBase packages';
		addComponentNamed: 'common/platform/gemstone_gemstoneBase/Components'
			toComponentNamed: 'common/Components';
		addPlatformNestedComponentNamed:
				'common/platform/gemstone_gemstoneBase/Specifications'
			condition:
				{'gemstone'.
					'gemstoneBase'}
			comment: 'Specifications gemstone or gemstoneBase packages';
		addComponentNamed: 'common/platform/gemstone_gemstoneBase/Specifications'
			toComponentNamed: 'common/Specifications';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed:
				'common/componentsV2/platform/gemstone/Components'
			condition: 'gemstone'
			comment: 'Components gemstone  packages ';
		addComponentNamed: 'common/componentsV2/platform/gemstone/Components'
			toComponentNamed: 'common/componentsV2/Components';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed: 'common/platform/gemstone/AST'
			condition: 'gemstone'
			comment: 'AST gemstone  packages ';
		addComponentNamed: 'common/platform/gemstone/AST'
			toComponentNamed: 'common/AST';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed: 'common/platform/gemstone/Loader'
			condition: 'gemstone'
			comment: 'Loader gemstone  packages ';
		addComponentNamed: 'common/platform/gemstone/Loader'
			toComponentNamed: 'common/Loader';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed: 'common/platform/gemstone/Core'
			condition: 'gemstone'
			comment: 'Core gemstone  packages ';
		addComponentNamed: 'common/platform/gemstone/Core'
			toComponentNamed: 'common/Core';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed: 'common/platform/gemstone/Definitions'
			condition: 'gemstone'
			comment: 'Definitions gemstone  packages ';
		addComponentNamed: 'common/platform/gemstone/Definitions'
			toComponentNamed: 'common/Definitions';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed: 'common/platform/gemstone/Tools'
			condition: 'gemstone'
			comment: 'Tools gemstone  packages ';
		addComponentNamed: 'common/platform/gemstone/Tools'
			toComponentNamed: 'common/Tools';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed: 'common/platform/gemstone32-/Core'
			condition: 'gs3.[2-]'
			comment: 'Core gs3.[2-]  packages ';
		addComponentNamed: 'common/platform/gemstone32-/Core'
			toComponentNamed: 'common/Core';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed: 'common/platform/gemstone32-5/Core'
			condition: 'gs3.[2-5]'
			comment: 'Core gs3.[2-5]  packages ';
		addComponentNamed: 'common/platform/gemstone32-5/Core'
			toComponentNamed: 'common/Core';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed: 'common/platform/gemstone35-/Core'
			condition: 'gs3.[5-]'
			comment: 'Core gs3.[5-]  packages ';
		addComponentNamed: 'common/platform/gemstone35-/Core'
			toComponentNamed: 'common/Core';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed: 'common/platform/gemstone36-/Core'
			condition: 'gs3.[6-]'
			comment: 'Core gs3.[6-]  packages ';
		addComponentNamed: 'common/platform/gemstone36-/Core'
			toComponentNamed: 'common/Core';
		yourself
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _createGemStoneTestComponents: resolvedProject [
	resolvedProject
		addSimpleNestedComponentNamed: 'common/tests/platform/gemstone/Services'
			condition: 'gemstone'
			comment: 'Services gemstone test packages';
		addComponentNamed: 'common/tests/platform/gemstone/Services'
			toComponentNamed: 'common/tests/Services';
		addSimpleNestedComponentNamed: 'common/tests/platform/gemstone/Tests'
			condition: 'gemstone'
			comment: 'Tests gemstone test packages';
		addComponentNamed: 'common/tests/platform/gemstone/Tests'
			toComponentNamed: 'common/tests/Tests';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed:
				'common/tests/componentsV2/platform/gemstone32-/Components'
			condition: 'gs3.[2-]'
			comment: 'Components gs3.[2-]  packages ';
		addComponentNamed: 'common/tests/componentsV2/platform/gemstone32-/Components'
			toComponentNamed: 'common/tests/componentsV2/Components';
		yourself
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _createRowanComponents: resolvedProject [
	resolvedProject
		addSimpleNestedComponentNamed: 'common/AST'
			condition: 'common'
			comment:
				'Organize all ast classes under one component. Shared between Rowan and RowanDiskAPI.';
		addComponentNamed: 'common/AST' toComponentNamed: 'Rowan';
		addComponentNamed: 'common/AST' toComponentNamed: 'RowanDiskAPI';
		addSimpleNestedComponentNamed: 'common/Specifications'
			condition: 'common'
			comment:
				'Organize all specification classes under one component. Shared between Rowan and RowanDiskAPI.';
		addComponentNamed: 'common/Specifications' toComponentNamed: 'Rowan';
		addComponentNamed: 'common/Specifications' toComponentNamed: 'RowanDiskAPI';
		addSimpleNestedComponentNamed: 'common/Components'
			condition: 'common'
			comment:
				'Organize all component classes under one component. Shared between Rowan and RowanDiskAPI.';
		addComponentNamed: 'common/Components' toComponentNamed: 'Rowan';
		addComponentNamed: 'common/Components' toComponentNamed: 'RowanDiskAPI';
		addSimpleNestedComponentNamed: 'common/Tools'
			condition: 'common'
			comment:
				'Organize all tool classes under one component. Shared between Rowan and RowanDiskAPI.';
		addComponentNamed: 'common/Tools' toComponentNamed: 'Rowan';
		addComponentNamed: 'common/Tools' toComponentNamed: 'RowanDiskAPI';
		addSimpleNestedComponentNamed: 'common/Core'
			condition: 'common'
			comment:
				'Organize all core classes under one component. Shared between Rowan and RowanDiskAPI.';
		addComponentNamed: 'common/Core' toComponentNamed: 'Rowan';
		addComponentNamed: 'common/Core' toComponentNamed: 'RowanDiskAPI';
		addSimpleNestedComponentNamed: 'common/Tests'
			condition: 'tests'
			comment:
				'Rowan System test component. Shared between Rowan and RowanDiskAPI.';
		addComponentNamed: 'common/Tests' toComponentNamed: 'Rowan';
		addComponentNamed: 'common/Tests' toComponentNamed: 'RowanDiskAPI';
		yourself.	"Components that are shared between Rowan and RowanDiskAPI"

	resolvedProject
		addSimpleNestedComponentNamed: 'common/Kernel'
			condition: 'common'
			comment:
				'Organize all kernel classes under one component. Kernel classes are those core 
	classes that are not present as part of the RowanDiskAPI, most notably, the class Rowan.';
		addComponentNamed: 'common/Kernel' toComponentNamed: 'Rowan';
		addSimpleNestedComponentNamed: 'common/Definitions'
			condition: 'common'
			comment: 'Organize all definition classes under one component';
		addComponentNamed: 'common/Definitions' toComponentNamed: 'Rowan';
		addSimpleNestedComponentNamed: 'common/Services'
			condition: 'common'
			comment: 'Organize all services classes under one component';
		addComponentNamed: 'common/Services' toComponentNamed: 'Rowan';
		addComponentNamed: 'common/Services' toComponentNamed: 'Rowan';
		addSimpleNestedComponentNamed: 'common/Loader'
			condition: 'common'
			comment: 'Organize all loader classes under one component';
		addComponentNamed: 'common/Loader' toComponentNamed: 'Rowan';
		yourself	"Components that are used only in Rowan"
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _createRowanTestComponents: resolvedProject [
	resolvedProject
		addSimpleNestedComponentNamed: 'common/tests/Core'
			condition: 'tests'
			comment: 'Organize all core tests under one component';
		addComponentNamed: 'common/tests/Core' toComponentNamed: 'common/Core';
		addSimpleNestedComponentNamed: 'common/tests/Specifications'
			condition: 'tests'
			comment: 'Organize all specification tests under one component';
		addComponentNamed: 'common/tests/Specifications'
			toComponentNamed: 'common/Specifications';
		addSimpleNestedComponentNamed: 'common/tests/Components'
			condition: 'tests'
			comment: 'Organize all component tests under one component';
		addComponentNamed: 'common/tests/Components'
			toComponentNamed: 'common/Components';
		addSimpleNestedComponentNamed: 'common/tests/Services'
			condition: 'tests'
			comment: 'Organize all service tests under one component';
		addComponentNamed: 'common/tests/Services'
			toComponentNamed: 'common/Services';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed: 'common/tests/Tests'
			condition: 'tests'
			comment: 'Organize all system tests under one component';
		addComponentNamed: 'common/tests/Tests' toComponentNamed: 'common/Tests';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed: 'common/tests/v2/Specifications'
			condition: 'v2'
			comment: 'Organize all specification tests under one component';
		addComponentNamed: 'common/tests/v2/Specifications'
			toComponentNamed: 'common/tests/Specifications';
		addSimpleNestedComponentNamed: 'common/tests/v2/Components'
			condition: 'v2'
			comment: 'Organize all component tests under one component';
		addComponentNamed: 'common/tests/v2/Components'
			toComponentNamed: 'common/tests/Components';
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
RwSimpleComponentRowanExperiment >> _createV1Components: resolvedProject [
	resolvedProject
		addSimpleNestedComponentNamed: 'common/v2/StrawMan_1'
			condition: 'v2'
			comment: 'Organize all strawman v1 classes under one component';
		addComponentNamed: 'common/v2/StrawMan_1' toComponentNamed: 'Rowan';
		addSimpleNestedComponentNamed: 'common/tests/v1/Tests'
			condition: 'tests'
			comment: 'Organize all test v1 classes under one component';
		addComponentNamed: 'common/tests/v1/Tests'
			toComponentNamed: 'common/tests/Tests';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed: 'common/v1/Core'
			condition: 'v1'
			comment: 'Organize all v1 core classes under one component';
		addComponentNamed: 'common/v1/Core' toComponentNamed: 'common/Core';
		addSimpleNestedComponentNamed: 'common/v1/Tools'
			condition: 'v1'
			comment: 'Organize all v1 tools classes under one component';
		addComponentNamed: 'common/v1/Tools' toComponentNamed: 'common/Tools';
		addSimpleNestedComponentNamed: 'common/v1/Specifications'
			condition: 'v1'
			comment: 'Organize all v1 Specifications classes under one component';
		addComponentNamed: 'common/v1/Specifications'
			toComponentNamed: 'common/Specifications';
		addSimpleNestedComponentNamed: 'common/v1/Loader'
			condition: 'v1'
			comment: 'Organize all v1 loader classes under one component';
		addComponentNamed: 'common/v1/Loader' toComponentNamed: 'common/Loader';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed: 'common/v1/Components'
			condition: 'v1'
			comment: 'Organize all v1 components classes under one component';
		addComponentNamed: 'common/v1/Components'
			toComponentNamed: 'common/Components';
		addSimpleNestedComponentNamed: 'common/v1/Definitions'
			condition: 'v1'
			comment: 'Organize all v1 Definitions classes under one component';
		addComponentNamed: 'common/v1/Definitions'
			toComponentNamed: 'common/Definitions';
		yourself.

	(resolvedProject componentNamed: 'common/v1/Specifications')
		addProjectNamed: 'Cypress';
		addProjectNamed: 'Tonel';
		yourself
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _createV1DeprecatedComponents: resolvedProject [
	resolvedProject
		addSimpleNestedComponentNamed: 'common/tests/v1/deprecated/Tests'
			condition: 'deprecated'
			comment: 'Organize all test v1 deprecated classes under one component';
		addComponentNamed: 'common/tests/v1/deprecated/Tests'
			toComponentNamed: 'common/tests/v1/Tests';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed: 'common/v1/deprecated/Loader'
			condition: 'deprecated'
			comment: 'Organize all deprecated loader classes under one component';
		addComponentNamed: 'common/v1/deprecated/Loader'
			toComponentNamed: 'common/v1/Loader';
		addSimpleNestedComponentNamed: 'common/v1/deprecated/Definitions'
			condition: 'deprecated'
			comment: 'Organize all deprecated definitions classes under one component';
		addComponentNamed: 'common/v1/deprecated/Definitions'
			toComponentNamed: 'common/v1/Definitions';
		addSimpleNestedComponentNamed: 'common/v1/deprecated/Tools'
			condition: 'deprecated'
			comment: 'Organize all deprecated tools classes under one component';
		addComponentNamed: 'common/v1/deprecated/Tools'
			toComponentNamed: 'common/v1/Tools';
		yourself
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _createV2Components: resolvedProject [
	resolvedProject
		addSimpleNestedComponentNamed: 'common/tests/v2/Tests'
			condition: 'v2'
			comment: 'Organize all v2 tests classes under one component';
		addComponentNamed: 'common/tests/v2/Tests' toComponentNamed: 'common/tests/Tests';
		addSimpleNestedComponentNamed: 'common/v2/Tools'
			condition: 'v2'
			comment: 'Organize all v2 tools classes under one component';
		addComponentNamed: 'common/v2/Tools' toComponentNamed: 'common/Tools';
		addSimpleNestedComponentNamed: 'common/v2/Definitions'
			condition: 'v2'
			comment: 'Organize all v2 definitions classes under one component';
		addComponentNamed: 'common/v2/Definitions'
			toComponentNamed: 'common/Definitions';
		addSimpleNestedComponentNamed: 'common/v2/Core'
			condition: 'v2'
			comment: 'Organize all v2 core classes under one component';
		addComponentNamed: 'common/v2/Core' toComponentNamed: 'common/Core';
		addSimpleNestedComponentNamed: 'common/v2/Specifications'
			condition: 'v2'
			comment: 'Organize all v2 specification classes under one component';
		addComponentNamed: 'common/v2/Specifications'
			toComponentNamed: 'common/Specifications';
		addSimpleNestedComponentNamed: 'common/v2/Components'
			condition: 'v2'
			comment: 'Organize all v2 component classes under one component';
		addComponentNamed: 'common/v2/Components'
			toComponentNamed: 'common/Components';
		yourself
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _populateCommonComponentPackages: resolvedProject [
	"
		subs/DiskConfigurationsCommon common/core
		subs/DiskConfigurationsCommonV2 common/tests
		subs/CommonCore common/core

		StrawMan_1 common/core
	"

	(resolvedProject componentNamed: 'common/v2/StrawMan_1')
		addPackageNames: {'Rowan-Strawman1'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Strawman1'
			setSymbolDictNameTo: 'RowanTools';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/Core')
		addPackageNames:
				{'Rowan-Core'.
					'Rowan-Url-Core'.
					'Rowan-Core-Conditional-Support'.
					'Rowan-Kernel'.
					'Rowan-Core-Definitions-Extensions'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Core-Conditional-Support'
			setSymbolDictNameTo: 'RowanTools';
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Core-Definitions-Extensions'
			setSymbolDictNameTo: 'RowanTools';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/Definitions')
		addPackageNames:
				{'Rowan-Definitions'.
					'Rowan-Definitions-Common'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Definitions'
			setSymbolDictNameTo: 'RowanTools';
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Definitions-Common'
			setSymbolDictNameTo: 'RowanTools';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/Specifications')
		addPackageNames: {'Rowan-Specifications'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Specifications'
			setSymbolDictNameTo: 'RowanTools';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/Tools')
		addPackageNames: {'Rowan-Tools-Core'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Tools-Core'
			setSymbolDictNameTo: 'RowanTools';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/tests/Core')
		addPackageNames: {'Rowan-Tests-Core'};
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/Tests')
		addPackageNames: {'Rowan-Tests'};
		validate;
		yourself
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _populateComponentsV2ComponentPackages: resolvedProject [
	"
		subs/CommonComponentV2	common/core
	"

	(resolvedProject componentNamed: 'common/componentsV2/Components')
		addPackageNames:
				{'Rowan-Tools-Extensions-Components'.
					'Rowan-Cypress-Components'.
					'Rowan-Components'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Tools-Extensions-Components'
			setSymbolDictNameTo: 'RowanTools';
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Cypress-Components'
			setSymbolDictNameTo: 'RowanTools';
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Components'
			setSymbolDictNameTo: 'RowanTools';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/tests/componentsV2/Components')
		addPackageNames: {'Rowan-Tests-Components'};
		validate;
		yourself
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _populateGemStoneComponentPackages: resolvedProject [
	"
		subs/DiskConfigurationsCommon gemstone/core
		subs/Services gemstone/core
		subs/Services gemstone/tests
		subs/DiskConfigurationsCommonV2 gemstone/core
		subs/CommonComponent	gemstone/core
		subs/CommonComponentV2	gemstone/core

		componentsV2 gemstone/tests

		RowanV1 tests/deprecated/gemstone/
		RowanV2 v2/gemstone
	"

	(resolvedProject componentNamed: 'common/v2/platform/gemstone/Core')
		addPackageNames: {'Rowan-GemStone-CoreV2'};
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/v2/platform/gemstone/Definitions')
		addPackageNames:
				{'Rowan-GemStone-DefinitionsV2'.
					'Rowan-GemStone-Definitions-ExtensionsV2'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-DefinitionsV2'
			setSymbolDictNameTo: 'RowanTools';
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-Definitions-ExtensionsV2'
			setSymbolDictNameTo: 'RowanTools';
		validate;
		yourself.

	(resolvedProject
		componentNamed: 'common/tests/v1/deprecated/platform/gemstone/Tests')
		addPackageNames: {'Rowan-Tests-Deprecated'};
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/v1/platform/gemstone/Specifications')
		addPackageNames: {'Rowan-GemStone-SpecificationsV1'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-SpecificationsV1'
			setSymbolDictNameTo: 'RowanTools';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/v1/platform/gemstone/Core')
		addPackageNames: {'Rowan-Url-Cypress'};
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/v1/platform/gemstone/Definitions')
		addPackageNames:
				{'Rowan-Cypress-Definitions'.
					'Rowan-GemStone-DefinitionsV1'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Cypress-Definitions'
			setSymbolDictNameTo: 'RowanLoader';
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-DefinitionsV1'
			setSymbolDictNameTo: 'RowanLoader';
		validate;
		yourself.

	(resolvedProject
		componentNamed: 'common/v1/deprecated/platform/gemstone/Loader')
		addPackageNames:
				{'Rowan-GemStone-Loader-Deprecated'.
					'Rowan-GemStone-Loader-Extensions-Deprecated'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-Loader-Deprecated'
			setSymbolDictNameTo: 'RowanLoader';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/platform/gemstone_gemstoneBase/Core')
		addPackageNames: {'Rowan-GemStone-Url'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-Url'
			setSymbolDictNameTo: 'Globals';
		validate;
		yourself.

	(resolvedProject
		componentNamed: 'common/platform/gemstone_gemstoneBase/Components')
		addPackageNames:
				{'Rowan-GemStone-Components-Kernel'.
					'Rowan-Components-Kernel'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-Components-Kernel'
			setSymbolDictNameTo: 'Globals';
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Components-Kernel'
			setSymbolDictNameTo: 'Globals';
		validate;
		yourself.

	(resolvedProject
		componentNamed: 'common/platform/gemstone_gemstoneBase/Specifications')
		addPackageNames: {'Rowan-GemStone-Specifications'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-Specifications'
			setSymbolDictNameTo: 'RowanTools';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/platform/gemstone/Services')
		addPackageNames:
				{'Rowan-Services-Core'.
					'Rowan-Services-Extensions'.
					'Rowan-JadeServer'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-JadeServer'
			setSymbolDictNameTo: 'UserGlobals';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/v2/platform/gemstoneBase/Components')
		addPackageNames: {'Rowan-GsBase-ComponentsV2'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GsBase-ComponentsV2'
			setSymbolDictNameTo: 'RowanTools';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/v2/platform/gemstone/Components')
		addPackageNames: {'Rowan-GemStone-ComponentsV2'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-ComponentsV2'
			setSymbolDictNameTo: 'RowanTools';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/tests/platform/gemstone/Services')
		addPackageNames: {'Rowan-Services-Tests'};
		validate;
		yourself.

	(resolvedProject
		componentNamed: 'common/componentsV2/platform/gemstone/Components')
		addPackageNames:
				{'Rowan-GemStone-Components'.
					'Rowan-GemStone-Components-Extensions'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-Components-Extensions'
			setSymbolDictNameTo: 'RowanTools';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/platform/gemstone/AST')
		addPackageNames:
				{'AST-Core'.
					'AST-Kernel-Core'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'AST-Kernel-Core'
			setSymbolDictNameTo: 'Globals';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/platform/gemstone/Core')
		addPackageNames:
				{'Rowan-GemStone-Core'.
					'Rowan-GemStone-Kernel'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-Kernel'
			setSymbolDictNameTo: 'RowanTools';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/platform/gemstone/Loader')
		addPackageNames:
				{'Rowan-GemStone-Loader'.
					'Rowan-GemStone-Loader-Extensions'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-Loader'
			setSymbolDictNameTo: 'Globals';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/platform/gemstone/Definitions')
		addPackageNames: {'Rowan-GemStone-Definitions'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-Definitions'
			setSymbolDictNameTo: 'RowanTools';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/platform/gemstone/Tools')
		addPackageNames:
				{'Rowan-Tools-GemStone'.
					'Rowan-Tools-Extensions-GemStone'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Tools-GemStone'
			setSymbolDictNameTo: 'RowanTools';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/tests/platform/gemstone/Tests')
		addPackageNames:
				{'AST-Tests-Core'.
					'AST-Kernel-Tests-Core'.
					'Rowan-Tests-GemStone'.
					'Rowan-DataCurator-Tests'};
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/platform/gemstone32-/Core')
		addPackageNames:
				{'GemStone-Interactions-Core'.
					'GemStone-Interactions-Kernel'.
					'Rowan-GemStone-3215'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'GemStone-Interactions-Core'
			setSymbolDictNameTo: 'Globals';
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'GemStone-Interactions-Kernel'
			setSymbolDictNameTo: 'Globals';
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-3215'
			setSymbolDictNameTo: 'Globals';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/platform/gemstone32-5/Core')
		addPackageNames: {'Rowan-GemStone-Kernel-32x'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-Kernel-32x'
			setSymbolDictNameTo: 'Globals';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/platform/gemstone35-/Core')
		addPackageNames: {'Rowan-GemStone-35x'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-35x'
			setSymbolDictNameTo: 'Globals';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/platform/gemstone36-/Core')
		addPackageNames: {'Rowan-GemStone-36x'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-36x'
			setSymbolDictNameTo: 'Globals';
		validate;
		yourself.

	(resolvedProject
		componentNamed: 'common/tests/componentsV2/platform/gemstone32-/Components')
		addPackageNames:
				{'Rowan-Tests-35x'.
					'Rowan-Tests-GemStone-35x'};
		validate;
		yourself
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _populateV1ComponentPackages: resolvedProject [
	"
		subs/CommonCore common/core
	"

	(resolvedProject componentNamed: 'common/v1/deprecated/Tools')
		addPackageNames: {'Rowan-Tools-Deprecated'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Tools-Deprecated'
			setSymbolDictNameTo: 'RowanTools';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/v1/deprecated/Definitions')
		addPackageNames: {'Rowan-Definitions-Deprecated'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Definitions-Deprecated'
			setSymbolDictNameTo: 'RowanTools';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/v1/Tools')
		addPackageNames:
				{'Rowan-Cypress-Core'.
					'Rowan-Tools-Kernel'.
					'Rowan-Tools-CoreV1'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Tools-CoreV1'
			setSymbolDictNameTo: 'RowanTools';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/v1/Specifications')
		addPackageNames:
				{'Rowan-Url-Extensions'.
					'Rowan-SpecificationsV1'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-SpecificationsV1'
			setSymbolDictNameTo: 'RowanTools';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/v1/Components')
		addPackageNames: {'Rowan-Configurations'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Configurations'
			setSymbolDictNameTo: 'RowanTools';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/v1/Definitions')
		addPackageNames:
				{'Rowan-DefinitionsV1-Kernel'.
					'Rowan-DefinitionsV1'.
					'Rowan-Core-DefinitionsV1'.
					'Rowan-Url-CoreV1'.
					'Rowan-Cypress-Kernel'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-DefinitionsV1'
			setSymbolDictNameTo: 'RowanTools';
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-DefinitionsV1-Kernel'
			setSymbolDictNameTo: 'Globals';
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Core-DefinitionsV1'
			setSymbolDictNameTo: 'RowanTools';
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Cypress-Kernel'
			setSymbolDictNameTo: 'Globals';
		validate;
		yourself
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _populateV2ComponentPackages: resolvedProject [
	"
		subs/DiskConfigurationsCommonV2 common/core
		RowanV2 v2/gemstone
	"

	(resolvedProject componentNamed: 'common/tests/v2/Tests')
		addPackageNames: {'Rowan-TestsV2'};
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/v2/Tools')
		addPackageNames: {'Rowan-Tools-CoreV2'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Tools-CoreV2'
			setSymbolDictNameTo: 'RowanTools';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/v2/Definitions')
		addPackageNames:
				{'Rowan-DefinitionsV2'.
					'Rowan-Core-DefinitionsV2'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-DefinitionsV2'
			setSymbolDictNameTo: 'RowanTools';
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Core-DefinitionsV2'
			setSymbolDictNameTo: 'RowanTools';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/v2/Core')
		addPackageNames: {'Rowan-CoreV2'};
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/v2/Components')
		addPackageNames: {'Rowan-ComponentsV2'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-ComponentsV2'
			setSymbolDictNameTo: 'RowanTools';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/v2/Specifications')
		addPackageNames: {'Rowan-SpecificationsV2'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-SpecificationsV2'
			setSymbolDictNameTo: 'RowanTools';
		validate;
		yourself.

	(resolvedProject componentNamed: 'common/tests/v2/Components')
		addPackageNames: {'Rowan-Tests-ComponentsV2'};
		validate;
		yourself.
	(resolvedProject componentNamed: 'common/tests/v2/Specifications')
		addPackageNames: {'Rowan-Tests-SpecificationsV2'};
		validate;
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

	| specArray resolvedProject loadSpec |
	true
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
		_createV1Components: resolvedProject;
		_createV1DeprecatedComponents: resolvedProject;
		_createComponentsV2Components: resolvedProject;
		_createGemStoneComponents: resolvedProject;
		_createGemStoneTestComponents: resolvedProject;
		_populateCommonComponentPackages: resolvedProject;
		_populateGemStoneComponentPackages: resolvedProject;
		_populateV1ComponentPackages: resolvedProject;
		_populateV2ComponentPackages: resolvedProject;
		_populateComponentsV2ComponentPackages: resolvedProject;
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
		groupNames: #();
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
					'v1'.
					'deprecated'};
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
					'componentsV2'};
		exportTo: resolvedProject specsRoot;
		yourself
]

{ #category : 'tests' }
RwSimpleComponentRowanExperiment >> testReadExperimentalRowanComponentStructure [
	"Read project and packages from disk."

	| resolvedProject loadSpec loadSpecUrl projectsHome conditionalAttributes |
	loadSpecUrl := 'file:$ROWAN_PROJECTS_HOME/Rowan/rowan/v2/proposed_specs/ComponentV2_proposed.ston'.
	projectsHome := '$ROWAN_PROJECTS_HOME'.
	conditionalAttributes := {'common'.
	'gemstone'.
	'gemstone-kernel'.
	('3.5.0' asRwGemStoneVersionNumber)	"want to read 3.5.0 packages"}.

	loadSpec := RwSpecification fromUrl: loadSpecUrl.

	resolvedProject := loadSpec
		projectsHome: projectsHome;
		resolve.

	self deny: resolvedProject packageNames isEmpty.

]