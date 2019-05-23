tests
testVisitProjectLoadConfiguration_gemstone

	| url rowanSpec config visitor packageNames packageMapSpecs packagePropertiesMap x |
	rowanSpec := self _rowanProjectSpecification.
	url := 'file:' , rowanSpec repositoryRootPath , '/test/configs/RowanProjectLoadConfiguration.ston'.

	config := RwAbstractProjectConfiguration fromUrl: url.

	visitor := RwProjectLoadConfigurationVisitor new
		platformAttributes: #( 'common' 'gemstone' );
		groupNames: #('core' 'tests');
		yourself.
	visitor visit: config.

	packageNames := visitor packageNames asArray sort.
	self assert: packageNames = (x := self _expectedLoadPackageNames_gemstone).

	packageMapSpecs := visitor packageMapSpecs.
	self assert: packageMapSpecs notNil.

	Rowan image currentUserId = 'SystemUser'
		ifTrue: [
			self assert: (packageMapSpecs at: #'defaultSymbolDictName') = 'RowanKernel'.
			self assert: (packageMapSpecs at: #'defaultUseSessionMethodsForExtensions') = false.

			packagePropertiesMap := packageMapSpecs at: #packageNameToPlatformPropertiesMap.
			#('Rowan-Cypress-Kernel' 'Rowan-GemStone-Kernel' 'Rowan-Tools-Kernel' 'GemStone-Interactions-Kernel' 'Rowan-GemStone-3215') 
				do: [:packageName | 
					| map | 
					map := packagePropertiesMap at: packageName.
					self assert: (map at: 'symbolDictName') = 'Globals' ].

			#('Rowan-GemStone-Loader') 
				do: [:packageName | 
					| map | 
					map := packagePropertiesMap at: packageName.
					self assert: (map at: 'symbolDictName') = 'RowanLoader' ] ]
		ifFalse: [
			"default"
			self assert: (packageMapSpecs at: #'defaultSymbolDictName') = 'UserGlobals'.
			self assert: (packageMapSpecs at: #'defaultUseSessionMethodsForExtensions') = false ]
