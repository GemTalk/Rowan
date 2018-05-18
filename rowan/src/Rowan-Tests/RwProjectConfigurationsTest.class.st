Class {
	#name : 'RwProjectConfigurationsTest',
	#superclass : 'RwAbstractTest',
	#category : 'Rowan-Tests'
}

{ #category : 'tests' }
RwProjectConfigurationsTest >> testBasicProjectCompoundConfiguration [

	| url rowanSpec config |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	url := 'file:' , rowanSpec repositoryRootPath , '/test/configs/RowanProjectCompoundConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	self assert: config class == RwProjectCompoundConfiguration

]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testBasicProjectLoadConfiguration [

	"https://github.com/dalehenrich/Rowan/issues/189"

	| url rowanSpec config |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	url := 'file:' , rowanSpec repositoryRootPath , '/test/configs/RowanProjectLoadConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	self assert: config class == RwProjectLoadConfiguration

]

{ #category : 'tests' }
RwProjectConfigurationsTest >> testVisitProjectCompoundConfiguration [

	| url rowanSpec config visitor |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	url := 'file:' , rowanSpec repositoryRootPath , '/test/configs/RowanProjectCompoundConfiguration.ston'.

	config := RwProjectCompoundConfiguration fromUrl: url.

	visitor := RwProjectConfigurationVisitor new.
	visitor visit: config.

	self halt.

]
