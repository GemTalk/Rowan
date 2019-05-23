tests
testHybridModifyExtensionCompileMethod

	| normalClass projectName packageNames packageName1 packageName2 fooMethod normalInstance protocol x barMethod |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions'.
	packageNames := {packageName1.
	packageName2}.
	self
		_loadSessionMethodExtensionProjectDefinition: projectName
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

	protocol := '*' , packageName2 asLowercase.

	fooMethod := normalClass rwCompileMethod: 'foo ^''foo''' category: protocol.
	barMethod := normalClass class
		rwCompileMethod: 'bar ^''bar'''
		category: protocol.

	self assert: normalClass bar = 'bar'.
	self
		assert:
			(x := normalClass class categoryOfSelector: #'bar') asString = protocol.
	self assert: barMethod rowanPackageName = packageName2.

	normalInstance := normalClass new.
	self assert: normalInstance foo = 'foo'.
	self assert: (x := normalClass categoryOfSelector: #'foo') asString = protocol.
	self assert: fooMethod rowanPackageName = packageName2.

	fooMethod := normalClass rwCompileMethod: 'foo ^''bar''' category: protocol.

	self assert: (x := normalInstance foo) = 'bar'.
	self assert: (x := normalClass categoryOfSelector: #'foo') asString = protocol.
	self assert: fooMethod rowanPackageName = packageName2.

	barMethod := normalClass class
		rwCompileMethod: 'bar ^''foo'''
		category: protocol.

	self assert: (x := normalClass bar) = 'foo'.
	self
		assert:
			(x := normalClass class categoryOfSelector: #'bar') asString = protocol.
	self assert: barMethod rowanPackageName = packageName2