tests
testHybridCompileMethodRemove

	| normalClass projectName packageNames packageName1 packageName2 fooMethod barMethod normalInstance |
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
		rwSubclass: 'SimpleHybridNormal3'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.

	fooMethod := normalClass rwCompileMethod: 'foo ^''foo''' category: 'accessing'.
	barMethod := normalClass class
		rwCompileMethod: 'bar ^''bar'''
		category: '*' , packageName2 asLowercase.

	self assert: normalClass bar = 'bar'.
	normalInstance := normalClass new.
	self assert: normalInstance foo = 'foo'.

	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName2.

	normalClass rwRemoveSelector: #'foo'.
	normalClass class rwRemoveSelector: #'bar'.

	self should: [ normalClass bar ] raise: MessageNotUnderstood.
	self should: [ normalInstance foo ] raise: MessageNotUnderstood.

	fooMethod := normalClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , packageName2 asLowercase.
	barMethod := normalClass class
		rwCompileMethod: 'bar ^''bar'''
		category: 'accessing'.

	self assert: fooMethod rowanPackageName = packageName2.
	self assert: barMethod rowanPackageName = packageName1.

	self assert: normalClass bar = 'bar'.
	self assert: normalInstance foo = 'foo'.

	normalClass rwRemoveSelector: #'foo'.
	normalClass class rwRemoveSelector: #'bar'.

	self should: [ normalClass bar ] raise: MessageNotUnderstood.
	self should: [ normalInstance foo ] raise: MessageNotUnderstood