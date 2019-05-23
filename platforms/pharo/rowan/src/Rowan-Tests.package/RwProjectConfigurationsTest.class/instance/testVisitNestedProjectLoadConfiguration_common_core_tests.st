tests
testVisitNestedProjectLoadConfiguration_common_core_tests

	| url rowanSpec config visitor packageNames configurationBasePath |
	rowanSpec := self _rowanProjectSpecification.
	configurationBasePath :=  rowanSpec repositoryRootPath , '/test/configs/'.
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
