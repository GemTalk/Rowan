tests
testDisownClass1

	"disown a class with a method"

	| packageTools projectName packageNames className packageName theClass fooMethod |
	packageTools := Rowan packageTools.
	projectName := 'DisProject'.
	packageName := 'Disown-Core'.
	packageNames := {packageName}.
	className := 'DisownedClass'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'project for testing package disown api'.

	theClass := Object
		rwSubclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName
		options: #().
	self assert: theClass rowanPackageName = packageName.

	fooMethod := theClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , packageName asLowercase.

	self assert: theClass new foo = 'foo'.
	self assert: theClass rowanPackageName = packageName.
	self assert: fooMethod rowanPackageName = packageName.

	packageTools disown
		disownClassNamed: className.

	theClass := Rowan globalNamed: className.
	self assert: theClass notNil.
	self assert: theClass new foo = 'foo'.
	self assert: theClass rowanPackageName = Rowan unpackagedName.
	self assert: fooMethod rowanPackageName = Rowan unpackagedName.

	packageTools adopt
		adoptClassNamed: className  intoPackageNamed: packageName.

	theClass := Rowan globalNamed: className.
	self assert: theClass notNil.
	self assert: theClass new foo = 'foo'.
	self assert: theClass rowanPackageName = packageName.
	self assert: fooMethod rowanPackageName = packageName.
