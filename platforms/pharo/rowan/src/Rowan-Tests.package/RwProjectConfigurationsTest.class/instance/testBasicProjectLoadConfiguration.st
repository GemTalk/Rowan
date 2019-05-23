tests
testBasicProjectLoadConfiguration

	"https://github.com/dalehenrich/Rowan/issues/189"

	| url rowanSpec config |
	rowanSpec := self _rowanProjectSpecification.
	url := 'file:' , rowanSpec repositoryRootPath , '/test/configs/RowanProjectLoadConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	self assert: config class == RwProjectLoadConfiguration
