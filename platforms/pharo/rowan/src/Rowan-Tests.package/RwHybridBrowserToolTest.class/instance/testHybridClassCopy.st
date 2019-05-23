tests
testHybridClassCopy

	|  normalClass projectName packageNames packageName1 packageName2 comment newClassName newClass className |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions'.
	packageNames := {packageName1.
	packageName2}.
	comment := 'a comment'.
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
	normalClass class
		rwCompileMethod: 'bar ^Bar'
		category: 'accessing'.
	normalClass class
		rwCompileMethod: 'baz ^baz'
		category: 'accessing'.
	normalClass rwComment: comment.

	self assert: normalClass new foo isNil.
	self assert: normalClass bar isNil.
	self assert: normalClass baz isNil.
	self assert: normalClass comment = comment.

	newClassName := 'SimpleHybridNormal1Copy'.
	newClass := Rowan projectTools browser copyClassNamed: className to: newClassName.

	self assert: newClass new foo isNil.
	self assert: newClass bar isNil.
	self assert: newClass baz isNil.
	self assert: newClass comment = comment.

	self assert: (Rowan globalNamed: className) == normalClass
