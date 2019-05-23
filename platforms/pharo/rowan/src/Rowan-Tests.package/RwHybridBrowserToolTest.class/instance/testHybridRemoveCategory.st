tests
testHybridRemoveCategory

	|  normalClass projectName packageNames packageName1 packageName2 className |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions1'.
	packageNames := {packageName1.
	packageName2}.
	className :=  'SimpleHybridNormal1'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.

	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #(foo fah)
		classVars: #(Bar)
		classInstVars: #(baz)
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.

	normalClass
		rwCompileMethod: 'foo1 ^foo'
		category: '*' , packageName2 asLowercase.
	normalClass
		rwCompileMethod: 'foo2 ^foo'
		category: '*' , packageName2 asLowercase.
	normalClass class
		rwCompileMethod: 'fah1 ^baz'
		category: '*' , packageName2 asLowercase.
	normalClass class
		rwCompileMethod: 'fah2 ^Bar'
		category: '*' , packageName2 asLowercase.
	normalClass
		rwCompileMethod: 'bar1 ^Bar'
		category: 'accessing'.
	normalClass class
		rwCompileMethod: 'bar2 ^baz'
		category: 'accessing'.
	normalClass
		rwCompileMethod: 'bee1 ^Bar'
		category: 'accessing'.
	normalClass class
		rwCompileMethod: 'bee2 ^baz'
		category: 'accessing'.

	self assert: (normalClass categoryOfSelector: #foo1) = ( '*' , packageName2 asLowercase) asSymbol.
	self assert: (normalClass categoryOfSelector: #foo2) = ( '*' , packageName2 asLowercase) asSymbol.
	self assert: (normalClass class categoryOfSelector: #fah1) = ( '*' , packageName2 asLowercase) asSymbol.
	self assert: (normalClass class categoryOfSelector: #fah2) = ( '*' , packageName2 asLowercase) asSymbol.
	self assert: (normalClass categoryOfSelector: #bar1) = #accessing.
	self assert: (normalClass class categoryOfSelector: #bar2) = #accessing.
	self assert: (normalClass categoryOfSelector: #bee1) = #accessing.
	self assert: (normalClass class categoryOfSelector: #bee2) = #accessing.

	self assert: normalClass categoryNames size = 2.
	self assert: normalClass class categoryNames size = 2.

	normalClass
		rwRemoveCategory: 'accessing';
		rwRemoveCategory: ( '*' , packageName2 asLowercase);
		yourself.

	normalClass class
		rwRemoveCategory: 'accessing';
		rwRemoveCategory: ( '*' , packageName2 asLowercase);
		yourself.

	self assert: normalClass categoryNames size = 0.
	self assert: normalClass class categoryNames size = 0.
