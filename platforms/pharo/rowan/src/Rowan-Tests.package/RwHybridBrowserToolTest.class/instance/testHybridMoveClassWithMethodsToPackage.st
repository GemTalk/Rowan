tests
testHybridMoveClassWithMethodsToPackage

	| normalClass projectName packageNames packageName1 packageName2 movedNormalClass fooMethod barMethod |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core1'.
	packageName2 := 'HybridA-Core2'.
	packageNames := {packageName1.
	packageName2}.
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

	self assert: normalClass rowanPackageName = packageName1.
	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName1.

	movedNormalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		options: #().

	fooMethod := movedNormalClass compiledMethodAt: #foo.
	barMethod := movedNormalClass class compiledMethodAt: #bar.


	self assert: movedNormalClass == normalClass.
	self assert: movedNormalClass rowanPackageName = packageName2.
	self assert: fooMethod rowanPackageName = packageName2.
	self assert: barMethod rowanPackageName = packageName2.
