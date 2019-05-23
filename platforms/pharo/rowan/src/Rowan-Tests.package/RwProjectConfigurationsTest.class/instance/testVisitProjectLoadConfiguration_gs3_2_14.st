tests
testVisitProjectLoadConfiguration_gs3_2_14

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
