tests
testHybridMoveMethod_185

	"tighten down on the bare minimum set of changes needed to reproduce an #185 issue exposed byt RwBrowserTooApiTest>>testMoveMethod"

	|  normalClass projectName packageNames packageName1 packageName2 packageName3 className fooMethod |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions1'.
	packageName3 := 'HybridA-Extensions2'.
	packageNames := {packageName1.
	packageName2.
	packageName3}.
	className :=  'SimpleHybridNormal1'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.

	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #(foo)
		classVars: #(Bar)
		classInstVars: #(baz)
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.

	fooMethod := normalClass
		rwCompileMethod: 'foo ^foo'
		category: '*' , packageName2 asLowercase.

	self assert: (normalClass categoryOfSelector: #foo) = ( '*' , packageName2 asLowercase) asSymbol.
	self assert: fooMethod rowanPackageName = packageName2.

	fooMethod := normalClass
		rwCompileMethod: 'foo ^foo'
		category: '*' , packageName3 asLowercase.

	self assert: (normalClass categoryOfSelector: #foo) = ( '*' , packageName3 asLowercase) asSymbol.
	self assert: fooMethod rowanPackageName = packageName3.

	fooMethod := normalClass
		rwCompileMethod: 'foo ^foo'
		category: '*' , packageName2 asLowercase.

	self assert: (normalClass categoryOfSelector: #foo) = ( '*' , packageName2 asLowercase) asSymbol.
	self assert: fooMethod rowanPackageName = packageName2.
