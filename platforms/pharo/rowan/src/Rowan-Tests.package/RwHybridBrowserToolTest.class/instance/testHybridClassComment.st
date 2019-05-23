tests
testHybridClassComment

	| normalClass projectName packageNames packageName comment |
	projectName := 'Hybrid Project A'.
	packageName := 'HybridA-Core'.
	packageNames := {packageName}.
	comment := 'a comment'.

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
		category: packageName
		options: #().
	self assert: normalClass rowanPackageName = packageName.

	normalClass rwComment: comment.
	self assert: normalClass comment = comment
