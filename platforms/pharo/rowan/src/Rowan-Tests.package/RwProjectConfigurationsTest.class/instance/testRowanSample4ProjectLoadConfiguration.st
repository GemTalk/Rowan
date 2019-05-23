tests
testRowanSample4ProjectLoadConfiguration

	| configurationUrl rowanSpec config visitor packageNames gemStoneVersion packageMapSpecs packagePropertiesMap|

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
