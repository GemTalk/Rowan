tests-issue 165
testIssue165

	"https://github.com/dalehenrich/Rowan/issues/165"

	| projectName packageName1 className symDictName theClass theSymbolDict myUserProfile mySymbolList theSymDictIndex | 
	projectName := 'Issue165_Project'.
	packageName1 := 'Issue165-Core'.
	className := 'Issue165Class'.
	symDictName := self _symbolDictionaryName2.

	self 
		_createLoadedProjectNamed: projectName 
		root: '/tmp/rowanIssuesProject/' 
		symbolDictionaryName: symDictName 
		validate: false.

	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.

	theClass := Object
		rwSubclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	theClass rwCompileMethod: 'foo ^1' category: 'accessing'.

	theSymbolDict := Rowan globalNamed: symDictName.
	self assert: theSymbolDict class == SymbolDictionary.			"symbol dictionary is accessible in my symbol list"

	self assert: (Rowan projectNames includes: projectName).		"project is visible"
	self assert: (Rowan packageNames includes: packageName1).	"package is visible"
	self assert: (Rowan globalNamed: className) notNil.				"class is visible"

	myUserProfile := System myUserProfile.
	mySymbolList := System myUserProfile symbolList.
	theSymDictIndex := mySymbolList indexOf: theSymbolDict.
	myUserProfile removeDictionaryAt: theSymDictIndex.				"remove symbol dictionary from my symbol list"

	self assert: (Rowan projectNames includes: projectName).		"project is visible"
	self deny: (Rowan packageNames includes: packageName1).	"package is NOT visible"
	self deny: (Rowan globalNamed: className) notNil.				"class is NOT visible"
