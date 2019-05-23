tests
testClassVarSystemDictionaryClone

	| projectName packageName1 className symDictName theOriginalClass theSymbolDict validationBlock clonedSymbolDict 
		clonedClass clonedSymDictName | 

	projectName := 'SystemDictionaryClone_Project'.
	packageName1 := 'SystemDictionaryClone-Core'.
	className := 'SystemDictionaryCloneClass'.
	symDictName := self _symbolDictionaryName2.
	clonedSymDictName :=  (symDictName, '_CLONE') asSymbol.

	self 
		_createLoadedProjectNamed: projectName 
		root: '/tmp/rowanIssuesProject/' 
		symbolDictionaryName: symDictName 
		validate: false.

	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.

	theOriginalClass := Object
		rwSubclass: className
		instVarNames: #(iv1)
		classVars: #(CV1)
		classInstVars: #(civ1)
		poolDictionaries: #()
		category: packageName1
		options: #().

	theOriginalClass class rwCompileMethod: 'cv1 ^CV1' category: 'accessing'.
	theOriginalClass class rwCompileMethod: 'cv1: x CV1 := x' category: 'accessing'.

	validationBlock := [:aClass :expectedValue |
		| x |
		self assert: (x := aClass cv1) = expectedValue.
	].

	theOriginalClass cv1: #one.

	validationBlock value: theOriginalClass value: #one.

	theSymbolDict := Rowan globalNamed: symDictName.

	clonedSymbolDict := theSymbolDict _rowanCloneSymbolDictionaryNamed: clonedSymDictName symbolList: GsCurrentSession currentSession symbolList.
	self assert: clonedSymbolDict name == clonedSymDictName.

	clonedClass := clonedSymbolDict at: className asSymbol.

	self assert: clonedClass ~~ theOriginalClass.
	self assert: clonedClass classHistory ~~ theOriginalClass classHistory.

	validationBlock value: clonedClass value: #one.
	validationBlock value: theOriginalClass value: #one.

	clonedClass cv1: #ONE.

	validationBlock value: clonedClass value: #ONE.
	validationBlock value: theOriginalClass value: #one.
