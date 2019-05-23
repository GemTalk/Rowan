tests
testHybridCompileMethodMoveToProtocol

	| normalClass projectName packageNames packageName1 packageName2 fooMethod barMethod normalInstance x |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions'.
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
	self assert: normalClass rowanPackageName = packageName1.

	fooMethod := normalClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , packageName1 asLowercase.
	barMethod := normalClass class
		rwCompileMethod: 'bar ^''bar'''
		category: 'accessing'.

	self assert: normalClass bar = 'bar'.
	normalInstance := normalClass new.
	self assert: normalInstance foo = 'foo'.
	self
		assert:
			(x := normalClass class categoryOfSelector: #'bar') asString = 'accessing'.
	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName1.

	barMethod := normalClass class
		rwCompileMethod: 'bar "" ^''bar'''
		category: 'new category'.

	self assert: normalClass bar = 'bar'.
	self
		assert:
			(x := normalClass class categoryOfSelector: #'bar') asString = 'new category'.
	self assert: barMethod rowanPackageName = packageName1