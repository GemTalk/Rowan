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
		addSimpleNestedComponentNamed: 'common/platform/gemstone/Services'
			condition: 'gemstone'
			comment: 'Services gemstone packages';
		addComponentNamed: 'common/platform/gemstone/Services'
			toComponentNamed: 'common/platform/Services';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed: 'common/v2/platform/gemstoneBase/Components'
			condition: 'gemstoneBase'
			comment: 'Components gemstoneBase packages';
		addComponentNamed: 'common/v2/platform/gemstoneBase/Components'
			toComponentNamed: 'common/v2/platform/Components';
		addSimpleNestedComponentNamed: 'common/v2/platform/gemstone/Components'
			condition: 'gemstone'
			comment: 'Components gemstone packages';
		addComponentNamed: 'common/v2/platform/gemstone/Components'
			toComponentNamed: 'common/v2/platform/Components';
		yourself.

	resolvedProject
		addPlatformNestedComponentNamed: 'common/platform/gemstone_gemstoneBase/Core'
			condition:
				{'gemstone'.
					'gemstoneBase'}
			comment: 'Core gemstone or gemstoneBase packages';
		addComponentNamed: 'platform/gemstone_gemstoneBase/Core'
			toComponentNamed: 'common/platform/Core';
		yourself.

	resolvedProject
		addPlatformNestedComponentNamed:
				'common/platform/gemstone_gemstoneBase/Components'
			condition:
				{'gemstone'.
					'gemstoneBase'}
			comment: 'Components gemstone or gemstoneBase packages';
		addComponentNamed: 'platform/gemstone_gemstoneBase/Components'
			toComponentNamed: 'common/platform/Components';
		addPlatformNestedComponentNamed:
				'common/platform/gemstone_gemstoneBase/Specifications'
			condition:
				{'gemstone'.
					'gemstoneBase'}
			comment: 'Specifications gemstone or gemstoneBase packages';
		addComponentNamed: 'common/platform/gemstone_gemstoneBase/Specifications'
			toComponentNamed: 'common/platform/Specifications';
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
		addComponentNamed: 'common/platform/gemstone32x/Core'
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
		yourself.
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
			comment: 'SystemTests gemstone test packages';
		addComponentNamed: 'common/tests/platform/gemstone/Tests'
			toComponentNamed: 'common/tests/platform/Tests';
		yourself
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _createPlatformComponents: resolvedProject [
	resolvedProject
		addSimpleNestedComponentNamed: 'common/platform/Services'
			condition: 'common'
			comment: 'Services platform packages';
		addComponentNamed: 'common/platform/Services' toComponentNamed: 'common/Services';
		addSimpleNestedComponentNamed: 'common/platform/Core'
			condition: 'common'
			comment: 'Core platform packages';
		addComponentNamed: 'common/platform/Core' toComponentNamed: 'common/Core';
		addSimpleNestedComponentNamed: 'common/platform/Components'
			condition: 'common'
			comment: 'Components platform packages';
		addComponentNamed: 'common/platform/Components'
			toComponentNamed: 'common/Components';
		addSimpleNestedComponentNamed: 'common/platform/Specifications'
			condition: 'common'
			comment: 'Specifications platform packages';
		addComponentNamed: 'common/platform/Specifications'
			toComponentNamed: 'common/Specifications';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed: 'common/v2/platform/Components'
			condition: 'v2'
			comment: 'Components v2/platform packages';
		addComponentNamed: 'common/v2/platform/Components'
			toComponentNamed: 'common/Components';
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
		addSimpleNestedComponentNamed: 'common/SystemTests'
			condition: 'tests'
			comment: 'Organize all system test classes under one component';
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
		addSimpleNestedComponentNamed: 'common/tests/platform/Tests'
			condition: 'tests'
			comment: 'Organize all platform sytems tests under one component';
		addComponentNamed: 'common/tests/platform/Tests'
			toComponentNamed: 'common/tests/Tests';
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
		addSimpleNestedComponentNamed: 'common/v1/Tools'
			condition: 'v1'
			comment: 'Organize all v1 tools classes under one component';
		addComponentNamed: 'common/v1/Tools'
			toComponentNamed: 'common/Tools';
		addSimpleNestedComponentNamed: 'common/v1/Specifications'
			condition: 'v1'
			comment: 'Organize all v1 Specifications classes under one component';
		addComponentNamed: 'common/v1/Specifications'
			toComponentNamed: 'common/Specifications';
		yourself.

	(resolvedProject componentNamed: 'common/v1/Specifications')
		addProjectNamed: 'Cypress';
		addProjectNamed: 'Tonel';
		yourself
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _createV2Components: resolvedProject [
	resolvedProject
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
	"

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
			andPackageName: 'Rowan-Specifications'
			setSymbolDictNameTo: 'RowanTools';
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Core-Definitions-Extensions'
			setSymbolDictNameTo: 'RowanTools';
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
		yourself.

	(resolvedProject componentNamed: 'common/Specifications')
		addPackageNames: {'Rowan-Specifications'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Specifications'
			setSymbolDictNameTo: 'RowanTools';
		yourself.

	(resolvedProject componentNamed: 'common/Tools')
		addPackageNames: {'Rowan-Tools-Core'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Tools-Core'
			setSymbolDictNameTo: 'RowanTools';
		yourself.

	(resolvedProject componentNamed: 'common/tests/Core')
		addPackageNames: {'Rowan-Tests-Core'};
		yourself.

	(resolvedProject componentNamed: 'common/SystemTests')
		addPackageNames: {'Rowan-Tests'};
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
		yourself.

	(resolvedProject componentNamed: 'common/tests/componentsV2/Components')
		addPackageNames: {'Rowan-Tests-Components'};
		yourself.
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
	"

	(resolvedProject componentNamed: 'common/platform/gemstone_gemstoneBase/Core')
		addPackageNames: {'Rowan-GemStone-Url'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-Url'
			setSymbolDictNameTo: 'Globals';
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
		yourself.

	(resolvedProject
		componentNamed: 'common/platform/gemstone_gemstoneBase/Specifications')
		addPackageNames: {'Rowan-GemStone-Specifications'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-Specifications'
			setSymbolDictNameTo: 'RowanTools';
		yourself.

	(resolvedProject componentNamed: 'common/platform/gemstone/Services')
		addPackageNames:
				{'Rowan-Services-Core'.
					'Rowan-Services-Extensions'.
					'Rowan-JadeServer'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-JadeServer'
			setSymbolDictNameTo: 'UserGlobals';
		yourself.

	(resolvedProject componentNamed: 'common/v2/platform/gemstoneBase/Components')
		addPackageNames: {'Rowan-GsBase-ComponentsV2'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GsBase-ComponentsV2'
			setSymbolDictNameTo: 'RowanTools';
		yourself.

	(resolvedProject componentNamed: 'common/v2/platform/gemstone/Components')
		addPackageNames: {'Rowan-GemStone-ComponentsV2'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-ComponentsV2'
			setSymbolDictNameTo: 'RowanTools';
		yourself.

	(resolvedProject componentNamed: 'common/tests/platform/gemstone/Services')
		addPackageNames: {'Rowan-Services-Tests'};
		yourself.

	(resolvedProject
		componentNamed: 'common/componentsV2/platform/gemstone/Components')
		addPackageNames:
				{'Rowan-GemStone-Components'.
					'Rowan-GemStone-Components-Extensions'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-Components-Extensions'
			setSymbolDictNameTo: 'RowanTools';
		yourself.

	(resolvedProject componentNamed: 'common/platform/gemstone/AST')
		addPackageNames:
				{'AST-Core'.
					'AST-Kernel-Core'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'AST-Kernel-Core'
			setSymbolDictNameTo: 'Globals';
		yourself.

	(resolvedProject componentNamed: 'common/platform/gemstone/Core')
		addPackageNames:
				{'Rowan-GemStone-Core'.
					'Rowan-GemStone-Kernel'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-Kernel'
			setSymbolDictNameTo: 'RowanTools';
		yourself.

	(resolvedProject componentNamed: 'common/platform/gemstone/Loader')
		addPackageNames:
				{'Rowan-GemStone-Loader'.
					'Rowan-GemStone-Loader-Extensions'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-Loader'
			setSymbolDictNameTo: 'Globals';
		yourself.

	(resolvedProject componentNamed: 'common/platform/gemstone/Definitions')
		addPackageNames: {'Rowan-GemStone-Definitions'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-Definitions'
			setSymbolDictNameTo: 'RowanTools';
		yourself.

	(resolvedProject componentNamed: 'common/platform/gemstone/Tools')
		addPackageNames:
				{'Rowan-Tools-GemStone'.
					'Rowan-Tools-Extensions-GemStone'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Tools-GemStone'
			setSymbolDictNameTo: 'RowanTools';
		yourself.

	(resolvedProject componentNamed: 'common/tests/platform/gemstone/Tests')
		addPackageNames:
				{'AST-Tests-Core'.
					'AST-Kernel-Tests-Core'.
					'Rowan-Tests-GemStone'.
					'Rowan-DataCurator-Tests'};
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
		yourself.

	(resolvedProject componentNamed: 'common/platform/gemstone32-5/Core')
		addPackageNames:
				{'Rowan-GemStone-Kernel-32x'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-Kernel-32x'
			setSymbolDictNameTo: 'Globals';
		yourself.

	(resolvedProject componentNamed: 'common/platform/gemstone35-/Core')
		addPackageNames:
				{'Rowan-GemStone-35x'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-35x'
			setSymbolDictNameTo: 'Globals';
		yourself.

	(resolvedProject componentNamed: 'common/platform/gemstone36-/Core')
		addPackageNames:
				{'Rowan-GemStone-36x'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-36x'
			setSymbolDictNameTo: 'Globals';
		yourself
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _populateV1ComponentPackages: resolvedProject [
	"
		subs/CommonCore common/core
	"

	(resolvedProject componentNamed: 'common/v1/Tools')
		addPackageNames: {'Rowan-Cypress-Core'};
		addPackageNames: {'Rowan-Tools-Kernel'};
		yourself.
	(resolvedProject componentNamed: 'common/v1/Specifications')
		addPackageNames: {'Rowan-Url-Extensions'};
		yourself
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _populateV2ComponentPackages: resolvedProject [
	"
		subs/DiskConfigurationsCommonV2 common/core
	"

	(resolvedProject componentNamed: 'common/v2/Core')
		addPackageNames: {'Rowan-CoreV2'};
		yourself.

	(resolvedProject componentNamed: 'common/v2/Components')
		addPackageNames: {'Rowan-ComponentsV2'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-ComponentsV2'
			setSymbolDictNameTo: 'RowanTools';
		yourself.

	(resolvedProject componentNamed: 'common/v2/Specifications')
		addPackageNames: {'Rowan-SpecificationsV2'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-SpecificationsV2'
			setSymbolDictNameTo: 'RowanTools';
		yourself.

	(resolvedProject componentNamed: 'common/tests/v2/Components')
		addPackageNames: {'Rowan-Tests-ComponentsV2'};
		yourself.
	(resolvedProject componentNamed: 'common/tests/v2/Specifications')
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
		_createV1Components: resolvedProject;
		_createComponentsV2Components: resolvedProject;
		_createPlatformComponents: resolvedProject;
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
					'componentsV2'};
		exportTo: resolvedProject specsRoot;
		yourself
]
