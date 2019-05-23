tests
testAdoptSymbolList_1

	"https://github.com/dalehenrich/Rowan/issues/208"

	| projectName packageNames className packageName theClass symDict symDictName instanceMethod classMethod symbolList packageNameMap
		excludedSymbolDictionaries |
	projectName := 'AdoptProject'.
	packageName := 'Adopt-Core'.
	packageNames := {packageName}.
	symDictName := self _symbolDictionaryName2.
	className := 'AdoptedClass'.
	symbolList := Rowan image symbolList.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.

	symDict := Rowan globalNamed: symDictName.

	"Use non-Rowan api to create class and methods"
	theClass := Object subclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().

	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.
	classMethod := theClass class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	self assert: theClass rowanProjectName = Rowan unpackagedName.
	self assert: instanceMethod rowanProjectName = Rowan unpackagedName.
	self assert: classMethod rowanProjectName = Rowan unpackagedName.

	packageNameMap := Dictionary new
		at: symDictName asString put: packageName;
		yourself.
	excludedSymbolDictionaries := symbolList reject: [:symbolDictionary | symbolDictionary name = symDictName ].
	Rowan packageTools adopt 
		adoptSymbolList: symbolList 
		excluding: excludedSymbolDictionaries 
		intoPackagesNamed: packageNameMap.

	self assert: theClass rowanProjectName = projectName.
	self assert: instanceMethod rowanProjectName = projectName.
	self assert: classMethod rowanProjectName = projectName.

	self assert: theClass rowanPackageName = packageName.
	self assert: instanceMethod rowanPackageName = packageName.
	self assert: classMethod rowanPackageName = packageName.
