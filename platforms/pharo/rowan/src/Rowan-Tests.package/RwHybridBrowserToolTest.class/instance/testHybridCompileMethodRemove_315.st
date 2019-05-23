tests
testHybridCompileMethodRemove_315

	"https://github.com/dalehenrich/Rowan/issues/315"

	| normalClass projectName packageNames packageName1 packageName2 fooMethod barMethod normalInstance
		symDict registry |
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

"add methods"
	fooMethod := normalClass rwCompileMethod: 'foo ^''foo''' category: 'accessing'.
	barMethod := normalClass class
		rwCompileMethod: 'bar ^''bar'''
		category: '*' , packageName2 asLowercase.

"validate"
	self assert: normalClass bar = 'bar'.
	normalInstance := normalClass new.
	self assert: normalInstance foo = 'foo'.

	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName2.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 2.

"remove methods"
	normalClass rwRemoveSelector: #'foo'.
	normalClass class rwRemoveSelector: #'bar'.

"validate"
	self should: [ normalClass bar ] raise: MessageNotUnderstood.
	self should: [ normalInstance foo ] raise: MessageNotUnderstood.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

"re-add methods"
	fooMethod := normalClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , packageName2 asLowercase.
	barMethod := normalClass class
		rwCompileMethod: 'bar ^''bar'''
		category: 'accessing'.

"validate"
	self assert: fooMethod rowanPackageName = packageName2.
	self assert: barMethod rowanPackageName = packageName1.

	self assert: normalClass bar = 'bar'.
	self assert: normalInstance foo = 'foo'.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 2.

"remove methods"
	normalClass rwRemoveSelector: #'foo'.
	normalClass class rwRemoveSelector: #'bar'.

"validate"
	self should: [ normalClass bar ] raise: MessageNotUnderstood.
	self should: [ normalInstance foo ] raise: MessageNotUnderstood.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.
