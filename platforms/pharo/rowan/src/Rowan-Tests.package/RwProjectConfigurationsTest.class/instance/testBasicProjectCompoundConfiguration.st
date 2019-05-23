tests
testBasicProjectCompoundConfiguration

	| url rowanSpec config |
	rowanSpec := self _rowanProjectSpecification.
	url := 'file:' , rowanSpec repositoryRootPath , '/test/configs/RowanProjectCompoundConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	self assert: config class == RwProjectCompoundConfiguration
