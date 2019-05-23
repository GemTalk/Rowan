tests
testDisownPackage1

	| packageTools projectName packageNames className packageName1 packageName2 barMethod fooMethod theClass x |
	packageTools := Rowan packageTools.
	projectName := 'Disown Browser'.
	packageName1 := 'Disown-Core'.
	packageName2 := 'Disown-Extensions1'.
	packageNames := {packageName1.
	packageName2}.
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
		category: packageName1
		options: #().
	self assert: theClass rowanPackageName = packageName1.

	fooMethod := theClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , packageName1 asLowercase.
	barMethod := theClass
		rwCompileMethod: 'bar ^''bar'''
		category: '*' , packageName2 asLowercase.

	self assert: theClass new foo = 'foo'.
	self assert: theClass new bar = 'bar'.
	self assert: theClass rowanPackageName = packageName1.
	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName2.
	self assert: (Rowan image loadedPackageNamed: packageName1 ifAbsent: []) notNil.

	packageTools disown
		disownPackageNamed: packageName1.

	theClass := Rowan globalNamed: className.
	self assert: theClass notNil.
	self assert: theClass new foo = 'foo'.
	self assert: theClass new bar = 'bar'.
	self assert: theClass rowanPackageName = Rowan unpackagedName.
	self assert: fooMethod rowanPackageName = Rowan unpackagedName.
	self assert: (x := barMethod rowanPackageName) = packageName2.
	self assert: (Rowan image loadedPackageNamed: packageName1 ifAbsent: []) isNil.

	Rowan projectTools browser
		addPackageNamed: packageName1 toProjectNamed: projectName.
	packageTools adopt
		adoptClassNamed: className  intoPackageNamed: packageName1.

	theClass := Rowan globalNamed: className.
	self assert: theClass notNil.
	self assert: theClass new foo = 'foo'.
	self assert: theClass new bar = 'bar'.
	self assert: theClass rowanPackageName = packageName1.
	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName2.
	self assert: (Rowan image loadedPackageNamed: packageName1 ifAbsent: []) notNil.
