tests
testHybridMoveClassWithMethodsAndExtensionMethodsToExtensionPackage

	| normalClass projectName packageNames packageName1 packageName2 packageName3 movedNormalClass fooMethod barMethod ext1Method ext2Method |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core1'.
	packageName2 := 'HybridA-Core2'.
	packageName3 := 'HybridA-Extensions'.
	packageNames := {packageName1. packageName2. packageName3}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.

	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	fooMethod := normalClass
		rwCompileMethod: 'foo ^''foo'''
		category: 'accessing'.
	barMethod := normalClass class
		rwCompileMethod: 'bar ^''bar'''
		category: 'accessing'.
	ext1Method := normalClass
		rwCompileMethod: 'ext1 ^''ext1'''
		category: '*', packageName3 asLowercase.
	ext2Method := normalClass class
		rwCompileMethod: 'ext2 ^''ext2'''
		category: '*', packageName3 asLowercase.

	self assert: normalClass new foo = 'foo'.
	self assert: normalClass new ext1 = 'ext1'.
	self assert: normalClass bar = 'bar'.
	self assert: normalClass ext2 = 'ext2'.

	self assert: normalClass rowanPackageName = packageName1.
	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName1.
	self assert: ext1Method rowanPackageName = packageName3.
	self assert: ext2Method rowanPackageName = packageName3.

	movedNormalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName3
		options: #().

	self assert: normalClass new foo = 'foo'.
	self assert: normalClass new ext1 = 'ext1'.
	self assert: normalClass bar = 'bar'.
	self assert: normalClass ext2 = 'ext2'.

	fooMethod := movedNormalClass compiledMethodAt: #foo.
	barMethod := movedNormalClass class compiledMethodAt: #bar.
	ext1Method := movedNormalClass compiledMethodAt: #ext1.
	ext2Method := movedNormalClass class compiledMethodAt: #ext2.

	self assert: movedNormalClass == normalClass.
	self assert: movedNormalClass rowanPackageName = packageName3.
	self assert: fooMethod rowanPackageName = packageName3.
	self assert: barMethod rowanPackageName = packageName3.
	self assert: ext1Method rowanPackageName = packageName3.
	self assert: ext2Method rowanPackageName = packageName3.
