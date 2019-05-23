tests
testHybridMoveMethod

	|  normalClass projectName packageNames packageName1 packageName2 packageName3 className |
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

	normalClass
		rwCompileMethod: 'foo ^foo'
		category: '*' , packageName2 asLowercase.
	normalClass
		rwCompileMethod: 'bar ^Bar'
		category: 'accessing'.
	normalClass class
		rwCompileMethod: 'baz ^baz'
		category: 'accessing'.

	self assert: (normalClass categoryOfSelector: #foo) = ( '*' , packageName2 asLowercase) asSymbol.
	self assert: (normalClass categoryOfSelector: #bar) = #accessing.
	self assert: (normalClass class categoryOfSelector: #baz) = #accessing.

	Rowan projectTools browser
		moveMethod: #'foo' forClassNamed: 'SimpleHybridNormal1' isMeta: false toProtocol: 'accessing';
		moveMethod: #'bar' forClassNamed: 'SimpleHybridNormal1' isMeta: false toProtocol: ( '*' , packageName2 asLowercase);
		moveMethod: #'baz' forClassNamed: 'SimpleHybridNormal1' isMeta: true toProtocol: ( '*' , packageName2 asLowercase);
		yourself.

	self assert: (normalClass categoryOfSelector: #foo) = #accessing.
	self assert: (normalClass categoryOfSelector: #bar) = ( '*' , packageName2 asLowercase) asSymbol.
	self assert: (normalClass class categoryOfSelector: #baz) = ( '*' , packageName2 asLowercase) asSymbol.

	Rowan projectTools browser
		moveMethod: #'foo' forClassNamed: 'SimpleHybridNormal1' isMeta: false toProtocol: ( '*' , packageName3 asLowercase);
		moveMethod: #'bar' forClassNamed: 'SimpleHybridNormal1' isMeta: false toProtocol: ( '*' , packageName3 asLowercase);
		moveMethod: #'baz' forClassNamed: 'SimpleHybridNormal1' isMeta: true toProtocol: ( '*' , packageName3 asLowercase);
		yourself.

	self assert: (normalClass categoryOfSelector: #foo) = ( '*' , packageName3 asLowercase) asSymbol.
	self assert: (normalClass categoryOfSelector: #bar) = ( '*' , packageName3 asLowercase) asSymbol.
	self assert: (normalClass class categoryOfSelector: #baz) = ( '*' , packageName3 asLowercase) asSymbol.
