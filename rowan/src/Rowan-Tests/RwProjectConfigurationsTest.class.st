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
			'Rowan-Specifications' 'Rowan-Tests' 'Rowan-Tools-Core' 'Rowan-Tools-Kernel' 'Rowan-Url-Core') sort

]

{ #category : 'private' }
RwProjectConfigurationsTest >> _expectedLoadPackageNames_common [

	^ #('Rowan-Url-Core' 'Rowan-Cypress-Core' 'Rowan-Cypress-Kernel' 'Rowan-Definitions' 'Rowan-Specifications' 'Rowan-Configurations'
			'Rowan-Core' 'Rowan-Tools-Core' 'Rowan-Tools-Kernel' 'Rowan-Tests') sort

]

{ #category : 'private' }
RwProjectConfigurationsTest >> _expectedLoadPackageNames_gemstone [

	^ (self _expectedLoadPackageNames_common , 
			#('Rowan-Services-Extensions' 'Rowan-Services-Core' 'Rowan-GemStone-Core' 'Rowan-GemStone-Kernel' 'Rowan-GemStone-Loader'
					'Rowan-GemStone-Loader-Extensions' 'Rowan-Services-Tests') ) sort

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
RwProjectConfigurationsTest >> _expectedRowanSampleProject4_LoadPackageNames [

	^ #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 'RowanSample4-GemStone-Tests') sort

]

{ #category : 'private' }
RwProjectConfigurationsTest >> _rowanProjectSpecification [

	^ (Rowan image loadedProjectNamed: 'Rowan') specification

]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testBasicProjectCompoundConfiguration [

	| url rowanSpec config |
	rowanSpec := self _rowanProjectSpecification.
	url := 'file:' , rowanSpec repositoryRootPath , '/test/configs/RowanProjectCompoundConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	self assert: config class == RwProjectCompoundConfiguration

]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testBasicProjectLoadConfiguration [

	"https://github.com/dalehenrich/Rowan/issues/189"

	| url rowanSpec config |
	rowanSpec := self _rowanProjectSpecification.
	url := 'file:' , rowanSpec repositoryRootPath , '/test/configs/RowanProjectLoadConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	self assert: config class == RwProjectLoadConfiguration

]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testRowanSample4ProjectLoadConfiguration [

	| configurationUrl rowanSpec config visitor packageNames gemStoneVersion |

	rowanSpec := self _rowanProjectSpecification.
	configurationUrl := 'file:' , rowanSpec repositoryRootPath , '/test/configs/RowanSampleProject4_LoadConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: configurationUrl.

	gemStoneVersion := System stoneVersionReport at: 'gsVersion'.
	visitor := RwProjectLoadConfigurationVisitor new
		platformAttributes: { 'common'. 'gemstone'. gemStoneVersion asRwGemStoneVersionNumber};
		groupNames: #('core' 'tests');
		yourself.
	visitor visit: config.

	packageNames := visitor packageNames asArray sort.

	self assert: packageNames = self _expectedRowanSampleProject4_LoadPackageNames.

]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testVisitProjectCompoundConfiguration [

	| url rowanSpec config visitor packageNames |
	rowanSpec := self _rowanProjectSpecification.
	url := 'file:' , rowanSpec repositoryRootPath , '/test/configs/RowanProjectCompoundConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	visitor := RwProjectLoadConfigurationVisitor new.
	visitor visit: config.

	packageNames := visitor packageNames asArray sort.

	self assert: packageNames = self _expectedCompoundPackageNames.

]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testVisitProjectLoadConfiguration_common [

	| url rowanSpec config visitor packageNames |
	rowanSpec := self _rowanProjectSpecification.
	url := 'file:' , rowanSpec repositoryRootPath , '/test/configs/RowanProjectLoadConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	visitor := RwProjectLoadConfigurationVisitor new
		platformAttributes: #('common');
		groupNames: #('core' 'tests');
		yourself.
	visitor visit: config.

	packageNames := visitor packageNames asArray sort.

	self assert: packageNames = self _expectedLoadPackageNames_common.

]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testVisitProjectLoadConfiguration_gemstone [

	| url rowanSpec config visitor packageNames |
	rowanSpec := self _rowanProjectSpecification.
	url := 'file:' , rowanSpec repositoryRootPath , '/test/configs/RowanProjectLoadConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	visitor := RwProjectLoadConfigurationVisitor new
		platformAttributes: #( 'common' 'gemstone' );
		groupNames: #('core' 'tests');
		yourself.
	visitor visit: config.

	packageNames := visitor packageNames asArray sort.

	self assert: packageNames = self _expectedLoadPackageNames_gemstone.

]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testVisitProjectLoadConfiguration_gs3_2_14 [

	| url rowanSpec config visitor packageNames |
	rowanSpec := self _rowanProjectSpecification.
	url := 'file:' , rowanSpec repositoryRootPath , '/test/configs/RowanProjectLoadConfiguration.ston'.

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

	| url rowanSpec config visitor packageNames x |
	rowanSpec := self _rowanProjectSpecification.
	url := 'file:' , rowanSpec repositoryRootPath , '/test/configs/RowanProjectLoadConfiguration.ston'.

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

	| url rowanSpec config visitor packageNames x |
	rowanSpec := self _rowanProjectSpecification.
	url := 'file:' , rowanSpec repositoryRootPath , '/test/configs/RowanProjectLoadConfiguration.ston'.

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

	| url rowanSpec config visitor packageNames x |
	rowanSpec := self _rowanProjectSpecification.
	url := 'file:' , rowanSpec repositoryRootPath , '/test/configs/RowanProjectLoadConfiguration.ston'.

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

	| url rowanSpec config visitor packageNames x |
	rowanSpec := self _rowanProjectSpecification.
	url := 'file:' , rowanSpec repositoryRootPath , '/test/configs/RowanProjectLoadConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	visitor := RwProjectLoadConfigurationVisitor new
		platformAttributes: { 'common'. 'gemstone'. '3.3.0' asRwGemStoneVersionNumber};
		groupNames: #('core' 'tests');
		yourself.
	visitor visit: config.

	packageNames := visitor packageNames asArray sort.

	self assert: packageNames = (x := self _expectedLoadPackageNames_gs3_3_0).

]
