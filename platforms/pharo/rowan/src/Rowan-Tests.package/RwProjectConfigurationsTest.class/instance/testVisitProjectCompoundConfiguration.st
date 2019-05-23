tests
testVisitProjectCompoundConfiguration

	| url rowanSpec config visitor packageNames |
	rowanSpec := self _rowanProjectSpecification.
	url := 'file:' , rowanSpec repositoryRootPath , '/test/configs/RowanProjectCompoundConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	visitor := RwProjectLoadConfigurationVisitor new.
	visitor visit: config.

	packageNames := visitor packageNames asArray sort.

	self assert: packageNames = self _expectedCompoundPackageNames.
