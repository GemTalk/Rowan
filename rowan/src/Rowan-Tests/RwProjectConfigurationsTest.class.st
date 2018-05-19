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
			'Rowan-Specifications' 'Rowan-Tests' 'Rowan-Tools-Core' 'Rowan-Tools-Kernel' 'Rowan-Url-Core')

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
RwProjectConfigurationsTest >> testVisitProjectCompoundConfiguration [

	| url rowanSpec config visitor packageNames |
	rowanSpec := self _rowanProjectSpecification.
	url := 'file:' , rowanSpec repositoryRootPath , '/test/configs/RowanProjectCompoundConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	visitor := RwProjectConfigurationVisitor new.
	visitor visit: config.

	packageNames := visitor packageNames asArray sort.

	self assert: packageNames = self _expectedCompoundPackageNames.

]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testVisitProjectLoadConfiguration [

	| url rowanSpec config visitor packageNames |
	rowanSpec := self _rowanProjectSpecification.
	url := 'file:' , rowanSpec repositoryRootPath , '/test/configs/RowanProjectLoadConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	visitor := RwProjectLoadConfigurationVisitor new.
	visitor visit: config.

	packageNames := visitor packageNames asArray sort.

	self assert: packageNames = self _expectedLoadPackageNames.

]
