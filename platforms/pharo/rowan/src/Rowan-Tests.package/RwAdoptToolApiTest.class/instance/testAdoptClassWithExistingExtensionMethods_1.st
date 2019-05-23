tests
testAdoptClassWithExistingExtensionMethods_1
	| projectName packageNames className packageName1 packageName2 theClass symDict symDictName fooMethod barMethod symbolList |
	projectName := 'AdoptProject'.
	packageName1 := 'Adopt-Core1'.
	packageName2 := 'Adopt-Core2'.
	packageNames := {packageName1. packageName2}.
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

	fooMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	barMethod := theClass
		rwCompileMethod: 'bar ^''bar'''
		category: '*' , packageName2 asLowercase.

	self assert: theClass rowanProjectName = Rowan unpackagedName.
	self assert: fooMethod rowanPackageName = Rowan unpackagedName.
	self assert: barMethod rowanPackageName = packageName2.

	Rowan packageTools adopt 
		adoptClassNamed: className 
		intoPackageNamed: packageName1.

	self assert: theClass rowanProjectName = projectName.
	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName2.
