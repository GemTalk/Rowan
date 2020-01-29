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
		addSimpleNestedComponentNamed: 'shared/platform/gemstone/Core'
			condition:
				{'gemstone'.
				'gemstoneBase'}
			groupName: 'core'
			comment: 'Core gemstone packages';
		addComponentNamed: 'platform/gemstone/Core'
			toComponentNamed: 'shared/org/Core';
		addSimpleNestedComponentNamed: 'shared/platform/gemstone/Components'
			condition:
				{'gemstone'.
				'gemstoneBase'}
			groupName: 'core'
			comment: 'Components gemstone packages';
		addComponentNamed: 'shared/platform/gemstone/Components'
			toComponentNamed: 'shared/org/Components';
		addSimpleNestedComponentNamed: 'shared/platform/gemstone/Specifications'
			condition:
				{'gemstone'.
				'gemstoneBase'}
			groupName: 'core'
			comment: 'Specifications gemstone packages';
		addComponentNamed: 'shared/platform/gemstone/Specifications'
			toComponentNamed: 'shared/org/Specifications';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed: 'platform/gemstone/Services'
			condition: {'gemstone'}
			groupName: 'core'
			comment: 'Services gemstone packages';
		addComponentNamed: 'platform/gemstone/Services'
			toComponentNamed: 'org/Services';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed: 'shared/platform/gemstoneBase/v2/Components'
			condition: {'gemstoneBase'}
			groupName: 'core'
			comment: 'Components gemstoneBase packages';
		addComponentNamed: 'shared/platform/gemstoneBase/v2/Components'
			toComponentNamed: 'shared/org/Components';
		addSimpleNestedComponentNamed: 'shared/platform/gemstone/v2/Components'
			condition: {'gemstone'}
			groupName: 'core'
			comment: 'Components gemstone packages';
		addComponentNamed: 'shared/platform/gemstone/v2/Components'
			toComponentNamed: 'shared/org/Components';
		yourself
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _createRowanComponents: resolvedProject [

	resolvedProject
		addSimpleNestedComponentNamed: 'org/Kernel'
			condition: 'common'
			groupName: 'core'
			comment: 'Organize all kernel classes under one component. Kernel classes are those core 
	classes that are not present as part of the RowanDiskAPI, most notably, the class Rowan.';
		addComponentNamed: 'org/Kernel' toComponentNamed: 'Rowan';
		addSimpleNestedComponentNamed: 'org/Definitions'
			condition: 'common'
			groupName: 'core'
			comment: 'Organize all definition classes under one component';
		addComponentNamed: 'org/Definitions' toComponentNamed: 'Rowan';
		addSimpleNestedComponentNamed: 'org/Services'
			condition: 'common'
			groupName: 'core'
			comment: 'Organize all services classes under one component';
		addComponentNamed: 'org/Services' toComponentNamed: 'Rowan';
		addSimpleNestedComponentNamed: 'org/Loader'
			condition: 'common'
			groupName: 'core'
			comment: 'Organize all loader classes under one component';
		addComponentNamed: 'org/Loader' toComponentNamed: 'Rowan';
		yourself.
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _createSharedComponents: resolvedProject [
	resolvedProject
		addSimpleNestedComponentNamed: 'shared/Shared'
			condition: 'common'
			groupName: 'core'
			comment:
				'Organize all components shared between Rowan and RowanDiskAPI top level components';
		addComponentNamed: 'shared/Shared' toComponentNamed: 'Rowan';
		addComponentNamed: 'shared/Shared' toComponentNamed: 'RowanDiskAPI';
		yourself.

	(resolvedProject componentNamed: 'shared/Shared')
		addProjectNamed: 'FileSystemGs';
		addProjectNamed: 'STON';
		yourself.

	resolvedProject
		addSimpleNestedComponentNamed: 'shared/org/Specifications'
			condition: 'common'
			groupName: 'core'
			comment: 'Organize all specification classes under one component';
		addComponentNamed: 'shared/org/Specifications' toComponentNamed: 'shared/Shared';
		addSimpleNestedComponentNamed: 'shared/org/Components'
			condition: 'common'
			groupName: 'core'
			comment: 'Organize all definition classes under one component';
		addComponentNamed: 'shared/org/Components' toComponentNamed: 'shared/Shared';
		addSimpleNestedComponentNamed: 'shared/org/Tools'
			condition: 'common'
			groupName: 'core'
			comment: 'Organize all tool classes under one component';
		addComponentNamed: 'shared/org/Tools' toComponentNamed: 'shared/Shared';
		addSimpleNestedComponentNamed: 'shared/org/Core'
			condition: 'common'
			groupName: 'core'
			comment: 'Organize all core classes under one component';
		addComponentNamed: 'shared/org/Core' toComponentNamed: 'shared/Shared';
		yourself
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _createTopLevelComponents: resolvedProject [
	resolvedProject
		addSimpleComponentNamed: 'Rowan'
			condition: 'common'
			groupName: 'core'
			comment: 'top-level component for loading entire Rowan project.';
		addSimpleComponentNamed: 'RowanDiskAPI'
			condition: 'common'
			groupName: 'core'
			comment:
				'top-level component for loading Rowan API for reading writing Rowan disk structure.';
		yourself.
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _createV2Components: resolvedProject [
	resolvedProject
		addSimpleNestedComponentNamed: 'shared/org/v2/Specifications'
			condition: 'v2'
			groupName: 'core'
			comment: 'Organize all v2 specification classes under one component';
		addComponentNamed: 'shared/org/v2/Specifications'
			toComponentNamed: 'shared/org/Specifications';
		addSimpleNestedComponentNamed: 'shared/org/v2/Components'
			condition: 'v2'
			groupName: 'core'
			comment: 'Organize all v2 component classes under one component';
		addComponentNamed: 'shared/org/v2/Components'
			toComponentNamed: 'shared/org/Components';
		yourself
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _populateCommonComponentPackages: resolvedProject [
	"
		subs/DiskConfigurationsCommon common/core
	"

	(resolvedProject componentNamed: 'shared/org/Core')
		addPackageNames:
				{'Rowan-Url-Core'.
					'Rowan-Configurations-Common'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Configurations-Common'
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

	(resolvedProject componentNamed: 'shared/org/Specifications')
		addPackageNames: {'Rowan-Specifications'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-Specifications'
			setSymbolDictNameTo: 'RowanTools';
		yourself
]

{ #category : 'private' }
RwSimpleComponentRowanExperiment >> _populateGemStoneComponentPackages: resolvedProject [
	"
		subs/DiskConfigurationsCommon gemstone/core
		subs/Services gemstone/core
		subs/DiskConfigurationsCommonV2 common/core
	"

	(resolvedProject componentNamed: 'shared/platform/gemstone/Core')
		addPackageNames: {'Rowan-GemStone-Url'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-Url'
			setSymbolDictNameTo: 'Globals';
		yourself.

	(resolvedProject componentNamed: 'shared/platform/gemstone/Components')
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

	(resolvedProject componentNamed: 'shared/platform/gemstone/Specifications')
		addPackageNames: {'Rowan-GemStone-Specifications'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GemStone-Specifications'
			setSymbolDictNameTo: 'RowanTools';
		yourself.

	(resolvedProject componentNamed: 'platform/gemstone/Services')
		addPackageNames:
				{'Rowan-Services-Core'.
					'Rowan-Services-Extensions'.
					'Rowan-JadeServer'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-JadeServer'
			setSymbolDictNameTo: 'UserGlobals';
		yourself.

	(resolvedProject componentNamed: 'shared/platform/gemstoneBase/v2/Components')
		addPackageNames:
				{'Rowan-GsBase-ComponentsV2'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-GsBase-ComponentsV2'
			setSymbolDictNameTo: 'RowanTools';
		yourself.

	(resolvedProject componentNamed: 'shared/platform/gemstone/v2/Components')
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

	(resolvedProject componentNamed: 'shared/org/v2/Components')
		addPackageNames: {'Rowan-ComponentsV2'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-ComponentsV2'
			setSymbolDictNameTo: 'RowanTools';
		yourself.

	(resolvedProject componentNamed: 'shared/org/v2/Specifications')
		addPackageNames: {'Rowan-SpecificationsV2'};
		conditionalPackageMapSpecsAtGemStoneUserId: 'allusers'
			andPackageName: 'Rowan-SpecificationsV2'
			setSymbolDictNameTo: 'RowanTools';
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
		_createSharedComponents: resolvedProject;
		_createRowanComponents: resolvedProject;
		_createGemStoneComponents: resolvedProject;
		_createV2Components: resolvedProject;
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
		groupNames: {'core'};
		comment: 'Load spec for loading Rowan v2';
		customConditionalAttributes: {'v2'};
		exportTo: resolvedProject specsRoot;
		yourself.

	loadSpec := loadSpec copy.
	loadSpec
		specName: 'RowanDiskAPI_proposed';
		componentNames: {'RowanDiskAPI'};
		comment: 'Load spec for loading Rowan disk API for v2';
		customConditionalAttributes: {'v2'};
		exportTo: resolvedProject specsRoot;
		yourself.

	loadSpec := loadSpec copy.
	loadSpec
		specName: 'RowanV1_proposed';
		componentNames: {'Rowan'};
		comment: 'Load spec for loading Rowan v1';
		customConditionalAttributes: {'v1'};
		exportTo: resolvedProject specsRoot;
		yourself.

	loadSpec := loadSpec copy.
	loadSpec
		specName: 'ComponentV2_proposed';
		componentNames: {'Rowan'};
		comment:
				'Load spec for loading Rowan Components v2 - current development image (v1, v2, components) components will be deprecated';
		customConditionalAttributes:
				{'v1'.
					'v2'.
					'components'};
		exportTo: resolvedProject specsRoot;
		yourself
]
