Class {
	#name : 'RwProjectConfigurationsTest',
	#superclass : 'TestCase',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwProjectConfigurationsTest >> _expectedCompoundPackageNames [

	^ #( 'Rowan-Configurations' 'Rowan-Core' 'Rowan-Cypress-Core' 'Rowan-Cypress-Kernel' 'Rowan-Definitions' 
			'Rowan-Deprecated' 'Rowan-GemStone-Core' 'Rowan-GemStone-Kernel' 'Rowan-GemStone-Loader' 
			'Rowan-GemStone-Loader-Extensions' 'Rowan-Services-Core' 'Rowan-Services-Extensions' 'Rowan-Services-Tests' 
			'Rowan-Specifications' 'Rowan-Tests' 'Rowan-Tools-Core' 'Rowan-Tools-Extensions' 'Rowan-Tools-Kernel' 'Rowan-Url-Core') sort
]

{ #category : 'private' }
RwProjectConfigurationsTest >> _expectedLoadPackageNames_common [

	^ #('Rowan-Url-Core' 'Rowan-Cypress-Core' 'Rowan-Cypress-Kernel' 'Rowan-Definitions' 
			'Rowan-Specifications' 'Rowan-Configurations' 'Rowan-Components-Kernel' 'Rowan-Components' 
			'Rowan-Core' 'Rowan-Tools-Core' 'Rowan-Tools-Kernel' 'Rowan-Tools-Extensions' 'Rowan-Tests') sort
]

{ #category : 'private' }
RwProjectConfigurationsTest >> _expectedLoadPackageNames_common_core [

	^ #('Rowan-Url-Core' 'Rowan-Cypress-Core' 'Rowan-Cypress-Kernel' 'Rowan-Definitions' 'Rowan-Specifications' 'Rowan-Configurations'
			'Rowan-Core' 'Rowan-Tools-Core' 'Rowan-Tools-Extensions' 'Rowan-Tools-Kernel') sort

]

{ #category : 'private' }
RwProjectConfigurationsTest >> _expectedLoadPackageNames_common_core_tests [

	^ (self _expectedLoadPackageNames_common_core, #('Rowan-Tests')) sort

]

{ #category : 'private' }
RwProjectConfigurationsTest >> _expectedLoadPackageNames_common_deprecated [

	^ (self _expectedLoadPackageNames_common_core , #('Rowan-Deprecated')) sort
]

{ #category : 'private' }
RwProjectConfigurationsTest >> _expectedLoadPackageNames_common_tests [

	^ self _expectedLoadPackageNames_common_core_tests

]

{ #category : 'private' }
RwProjectConfigurationsTest >> _expectedLoadPackageNames_gemstone [

	^ (self _expectedLoadPackageNames_common , 
			#('Rowan-Services-Extensions' 'Rowan-Services-Core' 'Rowan-GemStone-Core' 
					'Rowan-GemStone-Kernel' 'Rowan-GemStone-Loader' 'Rowan-GemStone-Components'
					'Rowan-GemStone-Components-Extensions'
					'Rowan-GemStone-Loader-Extensions' 'Rowan-Services-Tests' ) ) sort
]

{ #category : 'private' }
RwProjectConfigurationsTest >> _expectedLoadPackageNames_gs3_2_14 [

	^ (self _expectedLoadPackageNames_gemstone , 
			#('Rowan-GemStone-32' 'Rowan-Url-32' 'GemStone-Interactions-Core' 'GemStone-Interactions-Kernel') ) sort

]

{ #category : 'private' }
RwProjectConfigurationsTest >> _expectedLoadPackageNames_gs3_2_15 [

	^ (self _expectedLoadPackageNames_gemstone , 
			#('Rowan-GemStone-3215' 'Rowan-Url-3215' 'Rowan-GemStone-3215.x' 'Rowan-Url-3215.x' 'Rowan-GemStone-3215x' 'Rowan-Url-3215x' 'Rowan-GemStone-3215x17'
					'Rowan-Url-3215x17' 'Rowan-GemStone-32' 'Rowan-Url-32' 'GemStone-Interactions-Core' 'GemStone-Interactions-Kernel') ) sort

]

{ #category : 'private' }
RwProjectConfigurationsTest >> _expectedLoadPackageNames_gs3_2_16 [

	^ (self _expectedLoadPackageNames_gemstone , 
			#( 'Rowan-GemStone-3215x' 'Rowan-Url-3215x'  'Rowan-GemStone-3215x17'
					'Rowan-Url-3215x17' 'Rowan-GemStone-32' 'Rowan-Url-32' 'GemStone-Interactions-Core' 'GemStone-Interactions-Kernel') ) sort

]

{ #category : 'private' }
RwProjectConfigurationsTest >> _expectedLoadPackageNames_gs3_2_18 [

	^ (self _expectedLoadPackageNames_gemstone , 
			#('Rowan-GemStone-32' 'Rowan-Url-32' 'Rowan-GemStone-3215x' 'Rowan-Url-3215x' 'GemStone-Interactions-Core' 'GemStone-Interactions-Kernel') ) sort

]

{ #category : 'private' }
RwProjectConfigurationsTest >> _expectedLoadPackageNames_gs3_3_0 [

	^ (self _expectedLoadPackageNames_gemstone , 
			#( 'Rowan-GemStone-33' 'Rowan-Url-33' ) ) sort

]

{ #category : 'private' }
RwProjectConfigurationsTest >> _expectedRowan_LoadPackageNames [

	^ (self _expectedLoadPackageNames_gemstone , 
			self _expectedLoadPackageNames_gemstone_version, 
			#('AST-Core' 'AST-Kernel-Core' 'AST-Kernel-Tests-Core' 'AST-Tests-Core' 
				'Rowan-GemStone-3215' 'Rowan-Url-3215' 'GemStone-Interactions-Core' 
				'GemStone-Interactions-Kernel' 'Rowan-Url-Extensions' 'Rowan-Kernel' 
				'Rowan-GemStone-Specifications' 'Rowan-Core-Definitions-Extensions' 
				'Rowan-GemStone-Definitions' 'Rowan-Cypress-Definitions' 'Rowan-Url-Cypress' 
				'Rowan-Tools-GemStone' 'Rowan-Tools-Extensions-GemStone') ) sort
]

{ #category : 'private' }
RwProjectConfigurationsTest >> _expectedRowanSampleProject4_LoadPackageNames [

	^ #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 'RowanSample4-GemStone-Tests') sort

]

{ #category : 'private' }
RwProjectConfigurationsTest >> _rowanProject [

	^ Rowan image loadedProjectNamed: 'Rowan'
]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testBasicProjectCompoundConfiguration [

	| url rowanProject config |
	rowanProject := self _rowanProject.
	url := 'file:' , rowanProject repositoryRootPath , '/test/configs/RowanProjectCompoundConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	self assert: config class == RwProjectCompoundConfiguration
]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testBasicProjectLoadConfiguration [

	"https://github.com/dalehenrich/Rowan/issues/189"

	| url rowanProject config |
	rowanProject := self _rowanProject.
	url := 'file:' , rowanProject repositoryRootPath , '/test/configs/RowanProjectLoadConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	self assert: config class == RwProjectLoadConfiguration
]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testProjectCompoundConfigurationApi [

	| packageNames config visitor x packageMapSpecs packagePropertiesMap map |
	packageNames :=  #('p1' 'p2').
	config := RwProjectLoadConfiguration new
		name: 'Testing';
		addDefinedGroupName: 'core';
		addDefinedGroupName: 'deprecated' includeGroups: #( 'core' );
		addDefinedGroupName: 'tests' includeGroups: #( 'deprecated' 'core'  );
		conditionalPackagesAtConditions: {'common'}
			andGroup: 'core' 
			addPackageNames: packageNames;
		conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser' 
			setDefaultSymbolDictNameTo: 'RowanKernel';
		conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser' 
			andPackageName: (packageNames at: 1)
			setSymbolDictNameTo: 'SymDict1';
		conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser' 
			andPackageName: (packageNames at: 2)
			setSymbolDictNameTo: 'SymDict2';
		yourself.
	visitor := RwProjectLoadConfigurationVisitor new
		platformAttributes: { 'common'. 'gemstone'. (System stoneVersionReport at: 'gsVersion') asRwGemStoneVersionNumber};
		groupNames: #('core' 'tests');
		yourself.
	visitor visit: config.

	self assert: (x := visitor packageNames asArray sort) = packageNames.

	packageMapSpecs := visitor packageMapSpecs.
	self assert: packageMapSpecs notNil.

	self assert: (packageMapSpecs at: #'defaultSymbolDictName') = 'RowanKernel'.

	packagePropertiesMap := packageMapSpecs at: #packageNameToPlatformPropertiesMap.

	map := packagePropertiesMap at: (packageNames at: 1).
	self assert: (map at: 'symbolDictName') = 'SymDict1'.

	map := packagePropertiesMap at: (packageNames at: 2).
	self assert: (map at: 'symbolDictName') = 'SymDict2'.

	map := config conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser' andPackageName: (packageNames at: 1).
	self assert: (map at: 'symbolDictName') = 'SymDict1'.

	map := config conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser' andPackageName: (packageNames at: 2).
	self assert: (map at: 'symbolDictName') = 'SymDict2'.
]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testRowanLoadConfiguration [

	| configurationBasePath configurationUrl rowanProject config visitor packageNames gemStoneVersion packageMapSpecs packagePropertiesMap x |

	rowanProject := self _rowanProject.

	configurationBasePath :=  rowanProject repositoryRootPath , '/rowan/configs/'.
	configurationUrl := 'file:' , configurationBasePath, 'Load.ston'.

	config := RwAbstractProjectConfiguration fromUrl: configurationUrl.

	gemStoneVersion := System stoneVersionReport at: 'gsVersion'.
	visitor := RwProjectLoadConfigurationVisitor new
		platformAttributes: { 'common'. 'gemstone'. gemStoneVersion asRwGemStoneVersionNumber};
		groupNames: #('core' 'tests');
		configurationBasePath: configurationBasePath;
		yourself.
	visitor visit: config.

	packageNames := visitor packageNames asArray sort.

	self assert: packageNames = (x := (self _expectedRowan_LoadPackageNames, self _expectedLoadPackageNames_gemstone_tests) sort).

	packageMapSpecs := visitor packageMapSpecs.
	self assert: packageMapSpecs notNil.

	self assert: (packageMapSpecs at: #'defaultSymbolDictName') = 'RowanKernel'.
	self assert: (packageMapSpecs at: #'defaultUseSessionMethodsForExtensions') = false.

	packagePropertiesMap := packageMapSpecs at: #packageNameToPlatformPropertiesMap.
	#('Rowan-GemStone-Loader') 
		do: [:packageName | 
			| map | 
			map := packagePropertiesMap at: packageName.
			self assert: (map at: 'symbolDictName') = 'RowanLoader' ].
]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testRowanSample4ProjectLoadConfiguration [

	| configurationUrl rowanProject config visitor packageNames gemStoneVersion packageMapSpecs packagePropertiesMap|

	rowanProject := self _rowanProject.
	configurationUrl := 'file:' , rowanProject repositoryRootPath , '/test/configs/RowanSampleProject4_LoadConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: configurationUrl.

	gemStoneVersion := System stoneVersionReport at: 'gsVersion'.
	visitor := RwProjectLoadConfigurationVisitor new
		platformAttributes: { 'common'. 'gemstone'. gemStoneVersion asRwGemStoneVersionNumber};
		groupNames: #('core' 'tests');
		yourself.
	visitor visit: config.

	packageNames := visitor packageNames asArray sort.

	self assert: packageNames = self _expectedRowanSampleProject4_LoadPackageNames.

	packageMapSpecs := visitor packageMapSpecs.
	self assert: packageMapSpecs notNil.

	self assert: (packageMapSpecs at: #'defaultSymbolDictName') = 'RowanSample4SymbolDict'.
	self assert: (packageMapSpecs at: #'defaultUseSessionMethodsForExtensions') = false.

	packagePropertiesMap := packageMapSpecs at: #packageNameToPlatformPropertiesMap.
	#('RowanSample4-GemStone') 
		do: [:packageName | 
			| map | 
			map := packagePropertiesMap at: packageName.
			self assert: (map at: 'symbolDictName') = 'RowanSample4DictionarySymbolDict' ].
]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testVisitNestedProjectLoadConfiguration_common_core [

	| url rowanProject config visitor packageNames configurationBasePath |
	rowanProject := self _rowanProject.
	configurationBasePath :=  rowanProject repositoryRootPath , '/test/configs/'.
	url := 'file:' , configurationBasePath, 'RowanTopLevelProjectLoadConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	visitor := RwProjectLoadConfigurationVisitor new
		platformAttributes: #('common');
		groupNames: #('core');
		configurationBasePath: configurationBasePath;
		yourself.
	visitor visit: config.

	packageNames := visitor packageNames asArray sort.

	self assert: packageNames = self _expectedLoadPackageNames_common_core.
]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testVisitNestedProjectLoadConfiguration_common_core_tests [

	| url rowanProject config visitor packageNames configurationBasePath |
	rowanProject := self _rowanProject.
	configurationBasePath :=  rowanProject repositoryRootPath , '/test/configs/'.
	url := 'file:' , configurationBasePath, 'RowanTopLevelProjectLoadConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	visitor := RwProjectLoadConfigurationVisitor new
		platformAttributes: #('common');
		groupNames: #('core' 'tests');
		configurationBasePath: configurationBasePath;
		yourself.
	visitor visit: config.

	packageNames := visitor packageNames asArray sort.

	self assert: packageNames = self _expectedLoadPackageNames_common_core_tests.
]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testVisitNestedProjectLoadConfiguration_common_deprecated [

	| url rowanProject config visitor packageNames configurationBasePath |
	rowanProject := self _rowanProject.
	configurationBasePath :=  rowanProject repositoryRootPath , '/test/configs/'.
	url := 'file:' , configurationBasePath, 'RowanTopLevelProjectLoadConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	visitor := RwProjectLoadConfigurationVisitor new
		platformAttributes: #('common');
		groupNames: #('deprecated');
		configurationBasePath: configurationBasePath;
		yourself.
	visitor visit: config.

	packageNames := visitor packageNames asArray sort.

	self assert: packageNames = self _expectedLoadPackageNames_common_deprecated.
]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testVisitNestedProjectLoadConfiguration_common_tests [

	| url rowanProject config visitor packageNames configurationBasePath |
	rowanProject := self _rowanProject.
	configurationBasePath :=  rowanProject repositoryRootPath , '/test/configs/'.
	url := 'file:' , configurationBasePath, 'RowanTopLevelProjectLoadConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	visitor := RwProjectLoadConfigurationVisitor new
		platformAttributes: #('common');
		groupNames: #('tests');
		configurationBasePath: configurationBasePath;
		yourself.
	visitor visit: config.

	packageNames := visitor packageNames asArray sort.

	self assert: packageNames = self _expectedLoadPackageNames_common_tests.
]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testVisitNestedProjectLoadConfiguration_Master [

	"https://github.com/dalehenrich/Rowan/issues/252"

	| url rowanProject config visitor packageNames configurationBasePath |
	rowanProject := self _rowanProject.
	configurationBasePath :=  rowanProject repositoryRootPath , '/test/configs/'.
	url := 'file:' , configurationBasePath, 'RowanTopLevelProjectLoadConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	visitor := RwProjectLoadConfigurationVisitor new
		platformAttributes: #('common');
		groupNames: #('Master');
		configurationBasePath: configurationBasePath;
		yourself.
	visitor visit: config.

	packageNames := visitor packageNames asArray sort.

	self assert: packageNames = self _expectedLoadPackageNames_common_deprecated.
]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testVisitProjectCompoundConfiguration [

	| url rowanProject config visitor packageNames |
	rowanProject := self _rowanProject.
	url := 'file:' , rowanProject repositoryRootPath , '/test/configs/RowanProjectCompoundConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	visitor := RwProjectLoadConfigurationVisitor new.
	visitor visit: config.

	packageNames := visitor packageNames asArray sort.

	self assert: packageNames = self _expectedCompoundPackageNames.
]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testVisitProjectLoadConfiguration_common [

	| url rowanProject config visitor packageNames x |
	rowanProject := self _rowanProject.
	url := 'file:' , rowanProject repositoryRootPath , '/test/configs/RowanProjectLoadConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	visitor := RwProjectLoadConfigurationVisitor new
		platformAttributes: #('common');
		groupNames: #('core' 'tests');
		yourself.
	visitor visit: config.

	packageNames := visitor packageNames asArray sort.

	self assert: packageNames = (x := self _expectedLoadPackageNames_common).
]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testVisitProjectLoadConfiguration_gemstone [

	| url rowanProject config visitor packageNames packageMapSpecs packagePropertiesMap x |
	rowanProject := self _rowanProject.
	url := 'file:' , rowanProject repositoryRootPath , '/test/configs/RowanProjectLoadConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	visitor := RwProjectLoadConfigurationVisitor new
		platformAttributes: #( 'common' 'gemstone' );
		groupNames: #('core' 'tests');
		yourself.
	visitor visit: config.

	packageNames := visitor packageNames asArray sort.
	self assert: packageNames = (x := self _expectedLoadPackageNames_gemstone).

	packageMapSpecs := visitor packageMapSpecs.
	self assert: packageMapSpecs notNil.

	Rowan image currentUserId = 'SystemUser'
		ifTrue: [
			self assert: (packageMapSpecs at: #'defaultSymbolDictName') = 'RowanKernel'.
			self assert: (packageMapSpecs at: #'defaultUseSessionMethodsForExtensions') = false.

			packagePropertiesMap := packageMapSpecs at: #packageNameToPlatformPropertiesMap.
			#('Rowan-Cypress-Kernel' 'Rowan-GemStone-Kernel' 'Rowan-Tools-Kernel' 'GemStone-Interactions-Kernel' 'Rowan-GemStone-3215') 
				do: [:packageName | 
					| map | 
					map := packagePropertiesMap at: packageName.
					self assert: (map at: 'symbolDictName') = 'Globals' ].

			#('Rowan-GemStone-Loader') 
				do: [:packageName | 
					| map | 
					map := packagePropertiesMap at: packageName.
					self assert: (map at: 'symbolDictName') = 'RowanLoader' ] ]
		ifFalse: [
			"default"
			self assert: (packageMapSpecs at: #'defaultSymbolDictName') = 'UserGlobals'.
			self assert: (packageMapSpecs at: #'defaultUseSessionMethodsForExtensions') = false ]
]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testVisitProjectLoadConfiguration_gs3_2_14 [

	| url rowanProject config visitor packageNames |
	rowanProject := self _rowanProject.
	url := 'file:' , rowanProject repositoryRootPath , '/test/configs/RowanProjectLoadConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	visitor := RwProjectLoadConfigurationVisitor new
		platformAttributes: { 'common'. 'gemstone'. '3.2.14' asRwGemStoneVersionNumber};
		groupNames: #('core' 'tests');
		yourself.
	visitor visit: config.

	packageNames := visitor packageNames asArray sort.

	self assert: packageNames = self _expectedLoadPackageNames_gs3_2_14.
]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testVisitProjectLoadConfiguration_gs3_2_15 [

	| url rowanProject config visitor packageNames x |
	rowanProject := self _rowanProject.
	url := 'file:' , rowanProject repositoryRootPath , '/test/configs/RowanProjectLoadConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	visitor := RwProjectLoadConfigurationVisitor new
		platformAttributes: { 'common'. 'gemstone'. '3.2.15' asRwGemStoneVersionNumber};
		groupNames: #('core' 'tests');
		yourself.
	visitor visit: config.

	packageNames := visitor packageNames asArray sort.

	self assert: packageNames = (x := self _expectedLoadPackageNames_gs3_2_15).
]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testVisitProjectLoadConfiguration_gs3_2_16 [

	| url rowanProject config visitor packageNames x |
	rowanProject := self _rowanProject.
	url := 'file:' , rowanProject repositoryRootPath , '/test/configs/RowanProjectLoadConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	visitor := RwProjectLoadConfigurationVisitor new
		platformAttributes: { 'common'. 'gemstone'. '3.2.16' asRwGemStoneVersionNumber};
		groupNames: #('core' 'tests');
		yourself.
	visitor visit: config.

	packageNames := visitor packageNames asArray sort.

	self assert: packageNames = (x := self _expectedLoadPackageNames_gs3_2_16).
]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testVisitProjectLoadConfiguration_gs3_2_18 [

	| url rowanProject config visitor packageNames x |
	rowanProject := self _rowanProject.
	url := 'file:' , rowanProject repositoryRootPath , '/test/configs/RowanProjectLoadConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	visitor := RwProjectLoadConfigurationVisitor new
		platformAttributes: { 'common'. 'gemstone'. '3.2.18' asRwGemStoneVersionNumber};
		groupNames: #('core' 'tests');
		yourself.
	visitor visit: config.

	packageNames := visitor packageNames asArray sort.

	self assert: packageNames = (x := self _expectedLoadPackageNames_gs3_2_18).
]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testVisitProjectLoadConfiguration_gs3_3_0 [

	| url rowanProject config visitor packageNames x |
	rowanProject := self _rowanProject.
	url := 'file:' , rowanProject repositoryRootPath , '/test/configs/RowanProjectLoadConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	visitor := RwProjectLoadConfigurationVisitor new
		platformAttributes: { 'common'. 'gemstone'. '3.3.0' asRwGemStoneVersionNumber};
		groupNames: #('core' 'tests');
		yourself.
	visitor visit: config.

	packageNames := visitor packageNames asArray sort.

	self assert: packageNames = (x := self _expectedLoadPackageNames_gs3_3_0).
]
