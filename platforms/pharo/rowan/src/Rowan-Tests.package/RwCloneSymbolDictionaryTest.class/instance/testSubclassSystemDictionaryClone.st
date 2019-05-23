tests
testSubclassSystemDictionaryClone

	| projectName packageName1 className1 className2 symDictName theOriginalClass theSymbolDict clonedSymbolDict 
		clonedClass1 clonedClass2 clonedSymDictName theOriginalSubclass x | 

	projectName := 'SystemDictionaryClone_Project'.
	packageName1 := 'SystemDictionaryClone-Core'.
	className1 := 'SystemDictionaryCloneClass_1'.
	className2 := 'SystemDictionaryCloneClass_2'.
	symDictName := self _symbolDictionaryName2.
	clonedSymDictName :=  (symDictName, '_CLONE') asSymbol.

	self 
		_createLoadedProjectNamed: projectName 
		root: '/tmp/rowanIssuesProject/' 
		symbolDictionaryName: symDictName 
		validate: false.

	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.

	theOriginalClass := Object
		rwSubclass: className1
		instVarNames: #(iv1)
		classVars: #(CV1)
		classInstVars: #(civ1)
		poolDictionaries: #()
		category: packageName1
		options: #().
	theOriginalSubclass := theOriginalClass
		rwSubclass: className2
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	theSymbolDict := Rowan globalNamed: symDictName.

	clonedSymbolDict := theSymbolDict _rowanCloneSymbolDictionaryNamed: clonedSymDictName symbolList: GsCurrentSession currentSession symbolList.
	self assert: clonedSymbolDict name == clonedSymDictName.

	clonedClass1 := clonedSymbolDict at: className1 asSymbol.

	self assert: clonedClass1 ~~ theOriginalClass.
	self assert: clonedClass1 classHistory ~~ theOriginalClass classHistory.

	clonedClass2 := clonedSymbolDict at: className2 asSymbol.

	self assert: clonedClass2 ~~ theOriginalSubclass.
	self assert: clonedClass2 classHistory ~~ theOriginalSubclass classHistory.

	self assert: (x := clonedClass2 superclass) == clonedClass1
