tests
testRowanLoadConfiguration

	| configurationBasePath configurationUrl rowanSpec config visitor packageNames gemStoneVersion packageMapSpecs packagePropertiesMap x |

	rowanSpec := self _rowanProjectSpecification.

	configurationBasePath :=  rowanSpec repositoryRootPath , '/rowan/configs/'.
	configurationUrl := 'file:' , configurationBasePath, 'Load.ston'.

	config := RwAbstractProjectConfiguration fromUrl: configurationUrl.

	gemStoneVersion := System stoneVersionReport at: 'gsVersion'.
	visitor := RwProjectLoadConfigurationVisitor new
		platformAttributes: { 'common'. 'gemstone'. gemStoneVersion asRwGemStoneVersionNumber};
		groupNames: #('core' 'tests');
		configurationBasePath: configurationBasePath;
		yourself.
	visitor visit: config.

	packageNames := visitor packageNames asArray sort.

	self assert: packageNames = (x := self _expectedRowan_LoadPackageNames).

	packageMapSpecs := visitor packageMapSpecs.
	self assert: packageMapSpecs notNil.

	self assert: (packageMapSpecs at: #'defaultSymbolDictName') = 'RowanKernel'.
	self assert: (packageMapSpecs at: #'defaultUseSessionMethodsForExtensions') = false.

	packagePropertiesMap := packageMapSpecs at: #packageNameToPlatformPropertiesMap.
	#('Rowan-GemStone-Loader') 
		do: [:packageName | 
			| map | 
			map := packagePropertiesMap at: packageName.
			self assert: (map at: 'symbolDictName') = 'RowanLoader' ].
