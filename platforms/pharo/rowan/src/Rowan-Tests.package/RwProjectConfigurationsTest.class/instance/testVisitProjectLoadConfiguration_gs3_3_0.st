tests
testVisitProjectLoadConfiguration_gs3_3_0

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
