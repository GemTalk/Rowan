tests
testProjectCompoundConfigurationApi

	| packageNames config visitor x packageMapSpecs packagePropertiesMap map |
	packageNames :=  #('p1' 'p2').
	config := RwProjectLoadConfiguration new
		name: 'Testing';
		addDefinedGroupName: 'core';
		addDefinedGroupName: 'deprecated' includeGroups: #( 'core' );
		addDefinedGroupName: 'tests' includeGroups: #( 'deprecated' 'core'  );
		conditionalPackagesAtConditions: {'common'}
			andGroup: 'core' 
			addPackageNames: packageNames;
		conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser' 
			setDefaultSymbolDictNameTo: 'RowanKernel';
		conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser' 
			andPackageName: (packageNames at: 1)
			setSymbolDictNameTo: 'SymDict1';
		conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser' 
			andPackageName: (packageNames at: 2)
			setSymbolDictNameTo: 'SymDict2';
		yourself.
	visitor := RwProjectLoadConfigurationVisitor new
		platformAttributes: { 'common'. 'gemstone'. (System stoneVersionReport at: 'gsVersion') asRwGemStoneVersionNumber};
		groupNames: #('core' 'tests');
		yourself.
	visitor visit: config.

	self assert: (x := visitor packageNames asArray sort) = packageNames.

	packageMapSpecs := visitor packageMapSpecs.
	self assert: packageMapSpecs notNil.

	self assert: (packageMapSpecs at: #'defaultSymbolDictName') = 'RowanKernel'.

	packagePropertiesMap := packageMapSpecs at: #packageNameToPlatformPropertiesMap.

	map := packagePropertiesMap at: (packageNames at: 1).
	self assert: (map at: 'symbolDictName') = 'SymDict1'.

	map := packagePropertiesMap at: (packageNames at: 2).
	self assert: (map at: 'symbolDictName') = 'SymDict2'.

	map := config conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser' andPackageName: (packageNames at: 1).
	self assert: (map at: 'symbolDictName') = 'SymDict1'.

	map := config conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser' andPackageName: (packageNames at: 2).
	self assert: (map at: 'symbolDictName') = 'SymDict2'.
